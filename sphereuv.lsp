(defun c:sphereuv ()
  (setq divisions (getint "number of divisions, mod2 = 0::"))
  (setq parallels divisions)
  (setq meridians divisions)
  (setq radius 1)
  (setq spherepoints (list))
  (setq 2PI (* PI 2))

  ;;A function that draws a 3DFace. Pass in four xyz point lists
  (defun 3DFace	(p1 p2 p3 p4)
    (entmakex (list (cons 0 "3DFACE")
		    (cons 10 p1)
		    (cons 11 p2)
		    (cons 12 p3)
		    (cons 13 p4)
	      )
    )
  )

  (defun stoc (phi theta radius)
    (setq x (* radius (* (sin theta) (cos phi))))
    (setq y (* radius (* (sin theta) (sin phi))))
    (setq z (* radius (cos theta)))

    (setq pointl (list x y z))
    ;(princ pointl)
  )

  (defun wait (seconds / stop)
    (setq stop (+ (getvar "DATE") (/ seconds 86400.0)))
    (while (> stop (getvar "DATE")))
  )

  ;;A function that draws a 3DFace. Pass in four xyz point lists
  (defun 3DFace	(p1 p2 p3 p4)
    (entmakex (list (cons 0 "3DFACE")
		    (cons 10 p1)
		    (cons 11 p2)
		    (cons 12 p3)
		    (cons 13 p4)
	      )
    )
  )

  (setq p 0)
  (setq m 0)
  (repeat parallels
           
    (setq parallel (/ (* PI p) parallels))

    (repeat meridians

      (setq meridian (/ (* 2PI m) meridians))
      (setq point (stoc meridian parallel radius))
      (setq spherepoints (append spherepoints (list point)))
      (setq m (+ m 1))
    )
          
    (setq p (+ p 1))
  )




  (foreach point spherepoints
    (entmakex (list (cons 0 "POINT") (cons 10 point)))
  )



  (defun skinparallels (start)
    ;;loop this code for each iteration of parallels
					;(setq startp ( * parallels 2))

    (setq startp start)
    (setq s_topleft (nth startp spherepoints))
    (setq s_bottomleft (nth (+ meridians startp) spherepoints))

    (repeat (- meridians 1)
      (setq p_topleft (nth startp spherepoints))
      (setq p_topright (nth (+ startp 1) spherepoints))

      (setq belowp (+ meridians startp))

      (setq p_bottomleft (nth belowp spherepoints))
      (setq p_bottomright (nth (+ belowp 1) spherepoints))

      (3DFace p_topleft p_topright p_bottomleft p_topleft)
      (3DFace p_bottomleft p_bottomright p_topright p_bottomleft)

      (setq startp (+ startp 1))
    )

    ;;Hit the closure. Calculate
    (setq p_topleft (nth startp spherepoints))
    (setq P_topright s_topleft)

    (setq belowp (+ meridians startp))

    (setq p_bottomleft (nth belowp spherepoints))
    (setq p_bottomright s_bottomleft)

    (3DFace p_topleft p_topright p_bottomleft p_topleft)
    (3DFace p_bottomleft p_bottomright p_topright p_bottomleft)
  )


  (defun skinsouthpole (start)
    ;;loop this code for each iteration of parallels
					;(setq startp ( * parallels 2))

    (setq startp start)
	
    (setq s_topleft (nth startp spherepoints))
    (setq s_bottomleft (list 0 0 (* -1 radius)))

    (repeat (- meridians 1)
      (setq p_topleft (nth startp spherepoints))
      (setq p_topright (nth (+ startp 1) spherepoints))

      (setq p_bottomleft (list 0 0 (* -1 radius)))

      (3DFace p_topleft p_topright p_bottomleft p_topleft)
					;(3DFace p_bottomleft p_bottomright p_topright p_bottomleft)

      (setq startp (+ startp 1))
    )

    ;;Hit the closure. Calculate
    (setq p_topleft (nth startp spherepoints))
    (setq P_topright s_topleft)
    (setq p_bottomleft (list 0 0 (* -1 radius)))

    (3DFace p_topleft p_topright p_bottomleft p_topleft)
					;(3DFace p_bottomleft p_bottomright p_topright p_bottomleft)
  )





  (setq start 0)
  (setq par 0)
  (repeat (- parallels 1)
    (skinparallels par)
    (setq start (+ start 1))
    (setq par (* parallels start))
  )


  (setq start (* meridians (- parallels 1)))
  (skinsouthpole start)




  (print "done...")
)