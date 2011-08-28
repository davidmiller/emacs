;;
;; Lua configuration For Emacs
;;
(defext "\\.lua$"lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'hs-minor-mode)
(provide 'elua)