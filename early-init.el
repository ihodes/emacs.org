(setq package-enable-at-startup nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq initial-frame-alist `((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0)
			    (font . "Menlo-15")
                            (tool-bar-lines . 0)
			    (width . (text-pixels . 800))
                            (height . (text-pixels . 900))
                            (vertical-scroll-bars . nil)
                            (border-width . 0)))
