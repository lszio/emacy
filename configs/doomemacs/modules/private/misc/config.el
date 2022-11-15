(use-package! hammy
  :config
  (hammy-define (propertize "12" 'face '(:foreground))
    :documentation "Docs"
    :intervals
    (list
     (interval :name "Work"
               :duration "25 minutes"
               :before (do (announce "start work")
                           (notify "start work"))
               :advance (do (announce "break work")
                            (notify "break work"))))))
