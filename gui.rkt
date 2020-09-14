#lang racket
(require (planet jphelps/guiml) framework racket/gui "draw.rkt")
(provide input-gui)


; Helper Functions ---------------------------------------------------------------------------------
(define (make-range x y) (map number->string (stream->list (in-range x (add1 y)))))

(define (get-widget root node)
  (get-widget-by-id root node))

(define (get-choice-string root node)
  (send (get-widget root node) get-string-selection))

(define (get-choice-num root node)
  (string->number (get-choice-string root node)))

(define (get-text root node)
  (send (get-widget root node) get-value))


(define (get-date root node)
  (apply create-date (map string->number (string-split (get-text root node) "/"))))
   

(define (make-identifier string var)
  (string->symbol (format (string-append string "~s") var)))

(define (make-legend root node)
  (for/list ([i (get-choice-num root node)])
    (let ([name  (get-text root (make-identifier "name" i))]
          [start (get-date root (make-identifier "start-key" i))]
          [end   (get-date root (make-identifier "end-key" i))]
          [color (get-choice-string root (make-identifier "color-key" i))])
      (key name start end color))))

; GUI Objects --------------------------------------------------------------------------------------
;Time line is the object that displays the year picts
(define time-line%
  (class canvas%
    (inherit get-dc get-width get-height refresh-now)
    (init-field start-year end-year legend width height) 
    (define current-year 0)
    
    (define years (all-years start-year end-year legend width height))
    (super-new)

    ;On scroll overrided to change the current year in view on scroll either one year up or one year down.
    (define/override (on-scroll event)
      (set! current-year (send event get-position))
      (refresh-now))

    ;On-event overrided to give a popup menu on left click asking if you would like to export the timeline to a pdf form
    (define/override (on-event event)
      (when (send event button-up? 'right)
        (let ([menu (new popup-menu%
                         [title #f])]
              [x (send event get-x)]
              [y (send event get-y)])
          (new menu-item%
               [label "Create pdf?"]
               [parent menu]
               [callback
                (λ (c e)
                  (let ([file-name (get-text-from-user "Input File Name" "Name?")])
                    (create-pdf pdf-dc% years file-name width height)))]) 
          (send this popup-menu menu x y))))
        
    
    (define/override (on-paint)
      (define dc (get-dc))
      
     (draw-year (get-year years current-year) dc))
    ))

(define input-gui
  (class object%
    ; Possible colors a day can take, for selection from user.
    (define colors '("red" "green" "orange" "pink" "yellow" "light blue" "chocolate"
                     "CornflowerBlue" "MediumSlateBlue" "Gold" "Pale Green" "Aqua" "Khaki" "Thistle"  ))
      
    (define top-level
      (guiml (frame% 'frame
          (@ [label "interval"]
             [min-width 400]))))
    (add-gui-input!)
    (send top-level show #t)

    (super-new)

    (define/private (add-gui-input!)
      (add-children (get-widget-by-id top-level 'frame)
       (vertical-panel% 'gui-input-panel
         (@ [alignment '(center top)])
         (horizontal-panel%
           (@ [alignment '(center top)])
           (choice% 'start-year
             (@ [label "Start Year"]
                [style '(vertical-label)]
                [choices (make-range 2020 2040)]))
           (choice% 'end-year
             (@ [label "End Year"]
                [style '(vertical-label)]
                [choices (make-range 2020 2040)])))
           (choice% 'key-num
             (@ [label "How many keys in legend?"]
                [choices (make-range 1 (length colors))]
                [style '(vertical-label)]))
           (button% 'ok-button
             (@ [label "ok"]
                [callback
                 (λ (c e)
                   (when (get-widget top-level 'key-panel)
                     (remove-keys!))
                   (add-keys!))])))))

    (define/private (remove-gui-input!)
      (delete-children (get-widget top-level 'gui-input-panel)))

    (define/private (add-keys!)
      (add-children (get-widget top-level 'gui-input-panel)
        (vertical-panel% 'key-panel (@ [alignment '(center top)])))
        (for ([i (in-range 0 (get-choice-num top-level 'key-num))])
          (add-children (get-widget top-level 'key-panel)
            (horizontal-panel%
              (@ [alignment '(center top)])
              (text-field% (make-identifier "name" i)
                (@ [label "Name"]
                   [style '(single vertical-label)]))
              (text-field% (make-identifier "start-key" i)
                (@ [label "Start YYYY/MM/DD"]
                   [style '(single vertical-label)]))
              (text-field% (make-identifier "end-key" i)
                (@ [label "End YYYY/MM/DD"]
                   [style '(single vertical-label)]))
              (choice% (make-identifier "color-key" i)
                (@ [label "Color"]
                   [style '(vertical-label)]
                   [choices colors])))))
      (add-children (get-widget top-level 'key-panel)
        (button% (@ [label "done"]
                    [callback
                     (λ (c e)
                       (let ([legend (make-legend top-level 'key-num)]
                             [start  (get-choice-num top-level 'start-year)]
                             [end    (get-choice-num top-level 'end-year)])
                         (remove-gui-input!)
                         (add-preview! start end legend)))]))))

    (define/private (remove-keys!)
      (delete-children (get-widget top-level 'key-panel)))

    (define/private (add-preview! start-year end-year legend)
      (define-values (w h) (get-display-size))
      (add-children (get-widget top-level 'frame)
        (time-line% 'time-line
         (@ [start-year start-year]
            [end-year end-year]
            [legend legend]
            [style '(vscroll)]
            [width (* w 3/4)]
            [height (* h 3/4)])))
      (define frame (get-widget top-level 'time-line))
      (define year-num (length (make-range start-year end-year)))
      (if (> year-num 1)
        (send frame
              init-manual-scrollbars #f (sub1 year-num) 1 1 0 0)
        ;This is a temporary fix to the extra page problem, it wont work when I implement wheel scrolling
        (send frame
              init-manual-scrollbars #f #f 1 1 0 0))
      (send frame min-width (* w 3/4))
      (send frame min-height (* h 3/4)))

    ))          
#|
;Testing -------------------------------------------------------------------------------------------
(define-values (w h) (get-display-size))

(define width (* w 7/8))
(define height (* h 7/8))

(define top-level
  (new frame%
       [label "Interval"]
       [min-width width]
       [min-height height]))
(define time-line
  (new time-line%
       [parent top-level]
       [width width]
       [height height]
       [style '(vscroll)]
       [start-year 2020]
       [end-year 2022]
       [legend
        (list (key "a" (create-date 2020 3 3) (create-date 2021 4 4) "green")
              (key "b" (create-date 2020 5 5) (create-date 2021 7 7) "light blue")
              (key "c" (create-date 2021 6 6) (create-date 2022 4 4) "red"))]))
(send time-line init-manual-scrollbars #f  (abs (- 2022 2020)) 1 1 0 0)
(send top-level show #t)
 |#