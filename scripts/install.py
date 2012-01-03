#! /usr/bin/env python
#
# This script installs software on hosts on the network.
#
# Preconditions:
#   : Software must be built in advance of running this for more information.
#   : This must be invoked in the root directory of the source tree.
#   : User must be a member of the install group.
#   : This script does not account for concurrent installations to a common target host,
#     i.e. there is only one installation directory /usr/myproj. For concurrent user testing,
#     use the -i flag, to install your code only to a local tmp directory.
#   : Multiple users can install from the same host (to uncommon targets- ctrlcfg, bdataacq, etc.) concurrently,
#     but a given user cannot run multiple invocations of this script concurrently from the same host.
#
# How to prepare the system for this script, one time only(requires root/sudo privledges):
#   : /usr/sbin/groupadd -g 23000 install
#   : /usr/sbin/usermod -a -G install username
#   : /bin/mkdir /usr/myproj
#   : /bin/chgrp install /usr/myproj
#   : /bin/chmod -R g+w /usr/myproj
#   :This script does not do these steps to avoid having this script requiring escalating privs to
# root level.
#
# Here is an outline of what the script does:
#   : Does a 'make' with DESTDIR assigned to directory under /tmp/ .
#   : Creates README.install-info in the DESTDIR with information about the install.
#   : Uses rsync to copy the install directory to /usr/myproj/ on each host.

import getopt
import os
import time
import sys
import re
import subprocess

scriptName_g = re.sub(r'.*\/', '', sys.argv[0])
installPrefix_g = '/usr/myproj'
scriptHelp_g = """Usage: %s [options]
Options:
    -h, --help                          Print this help.
    -l, --host-list=HOST1,...,HOSTN     Input the list of hosts to install on.
    -i, --install-private-only          Performs private install only in users local tmp directory i.e.
                                        /tmp/install-smithj/. Skips installation to host-list.
                                        Use this for local test installations.
    -v, --version                       Print script version.""" % scriptName_g

(invoker_g, errStr) = subprocess.Popen('whoami', stdout=subprocess.PIPE, shell=True).communicate()
invoker_g = invoker_g.rstrip()
(localHostname_g, errStr) = subprocess.Popen('hostname', stdout=subprocess.PIPE, shell=True).communicate()
localHostname_g = localHostname_g.rstrip()

# If you change this, the first run of this script won't uninstall the right directory,
# so you should do it manually: make DESTDIR=oldDir uninstall
tmpInstallDir = '/tmp/install-%s' % invoker_g

#Installation on host by default
installPrivateBuild = 0

# Use this when invoking a shell command, and invoker doesn't care about the output
# except to error check the return as an errno.
def cmdOs(cmdStr, printCmd=False):
    if printCmd:
        print 'Issuing direct OS command: %s' % cmdStr
    osRet = os.system(cmdStr)
    if not osRet==0:
        raise Exception('System call failed with errno:%d: %s' % (osRet,cmdStr))

# Encapsulates a test string -- a set of hosts to test on.
class TestString:
    def __init__(self, testString):
        self.testString = testString
        self.hosts = []
        if testString=='localhost':
            self.hosts.append(localHostname_g)
        if testString=='det':
            self.hosts += ['dvswrksn', 'mdataacq', 'bdataacq']
        if testString=='beam':
            self.hosts += ['ccgwrksn', 'ctrlcfg', 'beamctrl']

# Create necessary installation directory structure locally.
#
# This doesn't require root privs.
def createLocalArtifacts(hostList):
    if( not os.path.exists(tmpInstallDir) ):
        cmdOs('mkdir %s' % tmpInstallDir)

    #
    # It's not clear the uninstall could properly delete files once the software is rebuilt,
    # if those files were removed from the build.
    #cmdOs('make DESTDIR=%s uninstall' % tmpInstallDir)
    # Same files that would have been installed to the system are diverted to tmpInstallDir.
    cmdOs('make DESTDIR=%s install' % tmpInstallDir)

    # Create README.install-info
    #
    (installTime, errStr) = subprocess.Popen("date '+%Y-%m-%dT%H:%M:%S.%N'", stdout=subprocess.PIPE, shell=True).communicate()
    installTime = installTime.rstrip()
    readmeText = 'Installed by user=%s at time=%s from host=%s and dir=%s to host-list=%s \n'%(
                invoker_g, installTime, localHostname_g, os.getcwd(), ','.join(hostList))
    os.system( "echo '%s' > %s%s/README.install-info" % (readmeText, tmpInstallDir, installPrefix_g) )

    cmdOs('chgrp -R install %s' % tmpInstallDir)
    cmdOs('chmod -R g+w %s' % tmpInstallDir)

    print( ' ' )
    print( '  Private Build created here: %s' % tmpInstallDir )


# Install software to remote host.
#
# Requires root privs.
def installOnHost(remoteHost):
    #
    # Doesn't use -a because of permissions issues with -p and -t.
    remoteCmd = 'rsync -rlgoD -v %s%s/ %s:%s/' % (tmpInstallDir, installPrefix_g, remoteHost, installPrefix_g)

    print( 'Executing remote command.' )
    print( '    user: %s' % invoker_g )
    print( '    host: %s' % remoteHost )
    print( '    command: %s' % remoteCmd )
    print( '    local source for rsync: %s' % tmpInstallDir )
    cmdOs(remoteCmd)

def __MAIN__():

    startTime = time.time()

    global installPrivateBuild

    # Default to localhost, override assignment if specified as optarg.
    hostList = [localHostname_g]


    # Process args
    #
    (opts, args) = getopt.getopt(sys.argv[1:], "hl:iev", ["help", "host-list=", "install-private-only", "version"])
    for optI, argI in opts:
        if optI in ('-h', '--help'):
            print(scriptHelp_g)
            sys.exit(0)
        if optI in ('-l', '--host-list'):
            hostList = argI.split(',')
        if optI in ('-i', '--install-private-only'):
            print 'Installing Private Build to %s:' % tmpInstallDir
            installPrivateBuild = 1
        if optI in ('-v', '--version'):
            print "You're on your own figuring out the version. "
            sys.exit(0)

    # The substance of the script
    #
    createLocalArtifacts(hostList)


    #when 0, install on all hostthe tmpInstallDir is installed on hosts
    #when 1  skip installing on any hosts
    if(installPrivateBuild==0):
       for hostI in hostList:
          installOnHost(hostI)

    # Finalities
    #
    stopTime = time.time()
    print('Completed installation of software successfully in %f seconds.' % (stopTime-startTime))
    for hostI in hostList:
       print( '  Installed on: %s' % hostI )

if __name__=='__main__':
    __MAIN__()

