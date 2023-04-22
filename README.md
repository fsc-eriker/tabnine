# tabnine

An unofficial TabNine package for Emacs.

## Installation

### straight-use-package

Add following code to your configuration.
```emacs
(use-package tabnine
  :hook (prog-mode . tabnine-mode)
  :straight (:host github :repo "shuxiao9058/tabnine")
  :hook (kill-emacs . tabnine-kill-process))
```

### manully
1. Install `tabnine`.

   Clone or download this repository.

   Add to your load path:

   ```emacs
   (add-to-list 'load-path "<path-to-tabnine>")
   (require 'tabnine)
   ```

2. Enable `tabnine-mode` in `prog-mode`.
   ```emacs
   (add-to-list 'prog-mode-hook #'tabnine-mode)
   ```

3. Run `M-x tabnine-install-binary` to install the TabNine binary for your system.

4. Recommend shortcut binding

```emacs
(define-key tabnine-mode-map (kbd "C-TAB") #'tabnine-accept-completion-by-word)
(define-key tabnine-mode-map (kbd "C-<tab>") #'tabnine-accept-completion-by-word)

(define-key tabnine-completion-map (kbd "TAB") #'tabnine-accept-completion)
(define-key tabnine-completion-map (kbd "<tab>") #'tabnine-accept-completion)

(define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
(define-key tabnine-completion-map (kbd "M-n") #'tabnine-next-completion)
(define-key tabnine-completion-map (kbd "M-p") #'tabnine-previous-completion)

(define-key tabnine-mode-map (kbd "TAB") #'tabnine-accept-completion)
(define-key tabnine-mode-map (kbd "<tab>") #'tabnine-accept-completion)
```

### Auto-balance parentheses

TabNine can automatically balance parentheses, by removing and adding closing parentheses after the cursor. See the examples [here](https://github.com/zxqfl/TabNine/blob/master/HowToWriteAClient.md).

## Known Issues

- TabNine's local deep learning completion might be enabled by default. It is very CPU-intensive if your device can't handle it. You can check by typing "TabNine::config" in any buffer (your browser should then automatically open to TabNine's config page) and disable Deep TabNine Local (you will lose local deep learning completion).

## Thanks

Thanks to the great work of [Tommy Xiang](https://github.com/TommyX12) and [zerolfx](https://github.com/zerolfx/copilot.el).
