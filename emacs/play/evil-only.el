;; -*- lexical-binding: t -*-

(setq load-path (append (file-expand-wildcards "~/.emacs.d/elpa/evil-*") load-path))
(require 'evil)
(evil-mode 1)
