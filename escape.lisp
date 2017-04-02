;;;; escape.lisp

(in-package #:escape)

(enable-block-syntax)

;;; "escape" goes here. Hacks and glory await!

;; (define-scene room01
;;     :background "images/room01.png"
;;     :right room02
;;     (:box "Box" "images/box.png" :x 560 :y 410)
;;     (:door "Door" "images/door.png" :x 300 :y 143
;;            :on-click
;;            ,(let (fired)
;;                  [(when (not fired)
;;                     (setf fired T)
;;                     (add *scene*
;;                          (make-sprite :knife "Knife" "images/knife.png" :x 100 :y 400
;;                                       :on-click [(setf *scene* (spawn (room02)))])))])))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro left-to (room)
    `(list :left "left" "images/arrowLeft.png" :x 10 :y 240
           :on-click [(switch-to-scene (,room))])))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro right-to (room)
    `(list :right "right" "images/arrowRight.png" :x 630 :y 240
           :on-click [(switch-to-scene (,room))])))


(defun room01 ()
  `(:room01 "images/room01.png"
            ,(right-to room02)
            (:box "Box" "images/box.png" :x 560 :y 410
                  :on-click ,[(add *inventory* (discard *scene* :box))] )
            (:portrait "Portrait" "images/portrait.png" :x 80 :y 150
                       :on-click ,[(when (selectedp :knife)
                                     (add *scene*
                                          (make-sprite :broken-portrait "Broken portrait"
                                                       "images/broken-portrait.png"
                                                       :x 80 :y 150))
                                     (add *scene*
                                          (make-sprite :lighter "Lighter"
                                                       "images/lighter.png"
                                                       :x 130 :y 400
                                                       :on-click
                                                       [(add *inventory*
                                                             (discard *scene* :lighter))]))
                                     (discard *inventory* :knife)
                                     (discard *scene* :portrait))])
            (:door "Door" "images/door.png" :x 300 :y 143
                   :on-click ,[ (when (selectedp :key)
                                  (sdl2:push-quit-event)) ])))

(defun room02 ()
  `(:room02 "images/room02.png"
            ,(left-to room01)
            ,(right-to room03)
            (:vase "Vase" "images/vase.png" :x 560 :y 410
                   :on-click ,[ (when (selectedp :hammer)
                                  (add *scene*
                                       (make-sprite :knife "Knife" "images/knife.png"
                                                    :x 560 :y 410
                                                    :on-click
                                                    [ (add *inventory*
                                                           (discard *scene* :knife)) ]))
                                  (discard *scene* :vase))])))

(defun room03 ()
  `(:room03 "images/room03.png"
            ,(left-to room02)
            ,(right-to room04)
            (:fireplace "Fireplace" "images/fireplace.png" :x 220 :y 144
                        :on-click
                        ,[(when (selectedp :logs)
                            (add *scene*
                                 (make-sprite :logs "Logs" "images/logs.png"
                                              :x 350 :y 338
                                              :on-click
                                              [ (when (selectedp :lighter)
                                                  (discard *scene* :logs)
                                                  (toggle (visible (@ *scene* :fire))))]))
                            (discard *inventory* :logs))])
            (:fire "Fire" "images/fire.png" :x 350 :y 325 :visible NIL
                   :on-click ,[(when (selectedp :box)
                                 (add *scene*
                                      (make-sprite :key "Key" "images/key.png"
                                                   :x 350 :y 338
                                                   :on-click
                                                   [(add *inventory* (discard *scene* :key))]))
                                 (discard *inventory* :box))])
            (:axe "Axe" "images/axe.png" :x 360 :y 180
                  :on-click ,[(add *inventory* (discard *scene* :axe))])
            (:toolbox "Toolbox" "images/toolbox.png" :x 120 :y 360
                      :on-click ,(once [(add *scene*
                                             (make-sprite :hammer "Hammer" "images/hammer.png"
                                                          :x 150 :y 420
                                                          :on-click
                                                          [ (add *inventory*
                                                                 (discard *scene* :hammer))]))]))))

(defun room04 ()
  `(:room04 "images/room04.png"
            ,(left-to room03)
            (:tree "Tree" "images/tree.png" :x 470 :y 100
                   :on-click ,[(when (selectedp :axe)
                                 (add *scene*
                                      (make-sprite :logs "Logs"
                                                   "images/logs.png"
                                                   :x 570 :y 350
                                                   :on-click
                                                   [(add *inventory*
                                                         (discard *scene* :logs))]))
                                 (discard *inventory* :axe)
                                 (discard *scene* :tree))])))

(defun run ()
  (when (not *vfs*) (build-vfs))
  (open-display))

(defun buildapp-entry-point (args)
  (declare (ignore args))
  (run))
