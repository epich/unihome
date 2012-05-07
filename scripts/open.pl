#! /usr/bin/perl -w

# open.pl
#
# Opens up .hxx and .cxx files in pairs over several dirs in the hardcoded editor.
# This script assumes the directory structure of RCCE.
# Files from several assemblies can be included, for example:
#     open.pl $RCCE_ROOT/Arch/Task $RCCE_ROOT/Cls/Lifecycle_Task
# would open up: Task.hxx Task.cxx Lifecycle_Task.hxx Lifecycle_Task.cxx
#

# Packages
use strict;
use Getopt::Long;

# Editor of preference, could make option but too tedious for users
# who prefer single editor
#my $editor = "/usr/local/bin/xemacs";
my $editor = `which vim`; chomp($editor);

# list of file types we should look for
#my @file_types = ("h","cpp");
my @file_types = ("idl","hxx","cxx","h","cpp");

# if -loose is used then you can search before and after the input parameter
# something like "Tdev_*File*.h"
my $wildcard = "";

# get options
my %options = ();
GetOptions(\%options, "loose");
if( $options{loose}) {
    $wildcard = "*";
}

# file_list string to pass to editor on command line
my $file_list = "";

# Redirect errors
open(STDERR, ">/dev/null");

# for each file passed by the user, find file types
foreach my $input_arg(@ARGV) {
    my $includeDir = "./include/";
    my $srcDir = "./src/";
    my $corbaDir = "./idl/";
    my $file = $input_arg;

    if( $input_arg =~ /(.*)\/(.*)/) {
        print STDOUT "Look for file $2 under directory $1\n";
        $includeDir = "$1/include/";
        $srcDir = "$1/src/";
        $corbaDir = "$1/idl/";
        $file = "$2";
    }

    foreach my $file_type (@file_types) {
        # Get a list in case there is more than one, for whatever reason
        my @files = `ls $includeDir$wildcard$file$wildcard.$file_type`; chomp(@files);
        push(@files, `ls $srcDir$wildcard$file$wildcard.$file_type`); chomp(@files);
        push(@files, `ls $corbaDir$wildcard$file$wildcard.$file_type`); chomp(@files);
        # for each file in the list, add it to our command line to open
        foreach my $file (@files) {
            $file_list .= "$file ";
        }
    }
}

if( $file_list eq "") {
    print "No files found.\n";
}
else {
    system("$editor $file_list");
}

