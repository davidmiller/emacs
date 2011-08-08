;;
;; Lua configuration For Emacs
;;
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'hs-minor-mode)
(provide 'elua)