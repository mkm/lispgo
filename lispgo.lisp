(in-package #:lispgo)

(defun bits-p (bits)
  (and (< (logand #xFF (ash bits 0)) 243)
       (< (logand #xFF (ash bits -8)) 243)
       (< (logand #xFF (ash bits -16)) 243)
       (< (logand #xFF (ash bits -24)) 243)
       (< (logand #xFF (ash bits -32)) 243)
       (< (logand #xFF (ash bits -40)) 243)
       (< (logand #xFF (ash bits -48)) 243)
       (< (logand #xFF (ash bits -56)) 243)))

(eval-always
  (defun trits-p (trits)
    (= 0 (logand trits (ash (logand #xAAAAAAAAAAAAAAAA trits) -1)))))

(defglobal +trit-to-bit+
  (loop :with arr = (make-array 683 :element-type '(unsigned-byte 8) :initial-element #xFF)
        :with val = 0
        :for trits :from 0 :below (array-dimension arr 0)
        :when (trits-p trits)
        :do (setf (aref arr trits) val) (incf val)
        :finally (return arr)))

(defglobal +bit-to-trit+
  (loop :with arr = (make-array 243 :element-type '(unsigned-byte 16) :initial-element #xFFFF)
        :for trits :from 0
        :and bits :across +trit-to-bit+
        :when (/= bits #xFF) :do (setf (aref arr bits) trits)
        :finally (return arr)))

(defun compress (trits)
  (declare (type (unsigned-byte 64) trits)
           (optimize (speed 3) (safety 0)))
  (macrolet ((comp (i) `(the (unsigned-byte 62)
                             (ash (aref +trit-to-bit+
                                        (logand #x3FF (ash trits ,(* -10 i))))
                                  ,(* 8 i)))))
    (logior (comp 0) (comp 1) (comp 2) (comp 3)
            (comp 4) (comp 5) (comp 6))))

(defun decompress (bits)
  (declare (type (unsigned-byte 64) bits)
           (optimize (speed 3) (safety 0)))
  (macrolet ((decomp (i) `(the (unsigned-byte 62)
                               (ash (aref +bit-to-trit+
                                          (logand #xFF (ash bits ,(* -8 i))))
                                    ,(* 10 i)))))
    (logior (decomp 0) (decomp 1) (decomp 2) (decomp 3)
            (decomp 4) (decomp 5) (decomp 6))))

(deftest test-compress-decompress
  (loop :for trits :from 0 :to 1000000
        :when (trits-p trits)
        :do (check (= (decompress (compress trits)) trits))))

(deftest test-decompress-compress
  (loop :for bits :from 0 :to 1000000
        :when (bits-p bits)
        :do (check (= (compress (decompress bits)) bits))))

(defun opponent (colour)
  (ecase colour
    (black 'white)
    (white 'black)))

(defmacro with-player (player &body body)
  (let ((args (gensym "ARGS")))
    `(ecase ,player
       (black (macrolet ((,player (&rest ,args) `(black ,@,args))) ,@body))
       (white (macrolet ((,player (&rest ,args) `(white ,@,args))) ,@body)))))

(defun colour-mask (colour)
  (ecase colour
    (black 0)
    (white #xFFFFFFFFFFFFFFFF)))

(deftype colour ()
  `(member black white))

(deftype bitmap ()
  '(unsigned-byte 62))

(eval-always
  (defun as-bitmap (bits)
    (declare (type integer bits))
    (logand #x3FFFFFFFFFFFFFFF bits)))

(eval-always
  (defun flip (bitmap)
    (declare (type bitmap bitmap)
             (optimize (speed 3) (safety 0)))
    (the bitmap (logandc2 most-positive-fixnum bitmap))))

(defun popcount (bitmap)
  (logcount bitmap))

(defconstant +ones+ (flip 0))

(defconstant +bitmap-left+ (as-bitmap #xFEFEFEFEFEFEFEFE))
(defconstant +bitmap-right+ (as-bitmap #x7F7F7F7F7F7F7F7F))
(defconstant +bitmap-top+ (as-bitmap #xFFFFFFFFFFFFFF00))
(defconstant +bitmap-bottom+ (as-bitmap #x00FFFFFFFFFFFFFF))

(defun from-left (bitmap)
  (declare (type bitmap bitmap))
  (as-bitmap (ash bitmap 1)))

(defun from-right (bitmap)
  (declare (type bitmap bitmap))
  (as-bitmap (ash bitmap -1)))

(defun from-top (bitmap)
  (declare (type bitmap bitmap))
  (as-bitmap (ash bitmap 8)))

(defun from-bottom (bitmap)
  (declare (type bitmap bitmap))
  (as-bitmap (ash bitmap -8)))

(defplace bit-at (bitmap row col)
  `(ldb (byte 1 (+ ,col (* ,row 8))) ,bitmap))

(defun bit-array (bitmap)
  (loop :with arr = (make-array '(8 8) :element-type 'bit :initial-element 0)
        :for i :from 0 :below 8
        :do (loop :for j :from 0 :below 8
                  :do (setf (aref arr i j) (bit-at bitmap i j)))
        :finally (return arr)))

(eval-always
  (defstruct (board :conc-name)
    (black 0 :type bitmap)
    (white 0 :type bitmap)))

(defmethod print-object ((board board) out)
  (format out "BOARD~A" (list (bit-array (black board)) (bit-array (white board)))))

(defglobal +empty-board+ (make-board :black 0 :white 0))

(defun valid-board-p (board)
  (declare (type board board))
  (= 0 (logand (black board) (white board))))

(eval-always
  (defstruct (frame :conc-name)
    (immortal (make-board) :type board)
    (vital (make-board) :type board)
    (edge 0 :type bitmap)
    (arena 0 :type bitmap)))

(defglobal +full-frame+
  (make-frame :immortal +empty-board+ :vital +empty-board+ :edge 0 :arena +ones+))

(deftype tile ()
  `(and symbol (member bb ww ib iw vb vw ee __ nn)))

(define-list-type tile-list tile)
(define-list-type tile-list-list tile-list)

~(defun tiles (frame board)
  "Returns an array of tile symbols representing a given frame and board."
  (loop :with arr = (make-array '(8 8) :element-type 'symbol :initial-element 'nn)
        :for row :from 0 :below 8
        :do (loop :for col :from 0 :below 8
                  :do (setf (aref arr row col) (tile frame board row col)))
        :finally (return arr)))

(defun make-tile-array (data)
  (loop :with arr = (make-array '(8 8) :element-type 'symbol :initial-element 'nn)
        :for row :from 0 :below 8
        :for row-data :in data
        :do (loop :for col :from 0 :below 8
                  :for tile :in row-data
                  :do (setf (aref arr row col) tile))
        :finally (return arr)))

(defun from-tiles (tiles)
  "Constructs a frame and a board from an array of tile symbols."
  (declare (type (or (simple-array t (8 8)) tile-list-list) tiles))
  (unless (arrayp tiles)
    (setf tiles (make-tile-array tiles)))
  (loop :with frame = (make-frame)
        :and board = (make-board)
        :for row :from 0 :below 8
        :do (loop :for col :from 0 :below 8
                  :when (or (< row 7) (< col 6))
                  :do (multiple-value-bind (b w ib iw vb vw e a)
                          (ecase (aref tiles row col)
                            (bb (values 1 0 0 0 0 0 0 1))
                            (ww (values 0 1 0 0 0 0 0 1))
                            (ib (values 1 0 1 0 0 0 0 1))
                            (iw (values 0 1 0 1 0 0 0 1))
                            (vb (values 1 0 0 0 1 0 0 1))
                            (vw (values 0 1 0 0 0 1 0 1))
                            (ee (values 0 0 0 0 0 0 1 0))
                            (__ (values 0 0 0 0 0 0 0 1))
                            (nn (values 0 0 0 0 0 0 0 0)))
                        (macrolet ((place (bitmap) `(bit-at ,bitmap row col)))
                          (setf (place (black board)) b
                                (place (white board)) w
                                (place (black (immortal frame))) ib
                                (place (white (immortal frame))) iw
                                (place (black (vital frame))) vb
                                (place (white (vital frame))) vw
                                (place (edge frame)) e
                                (place (arena frame)) a))))
        :finally (return (values frame board))))

~(multiple-value-bind (frame board)
    (from-tiles
      #2A((__ __ __ __ __ __ __ __)
          (__ __ bb __ __ __ __ __)
          (__ __ __ ww __ __ __ __)
          (__ __ __ __ __ __ __ __)
          (__ __ __ __ __ __ __ __)
          (__ __ __ __ __ __ __ __)
          (__ __ __ __ __ __ __ __)
          (__ __ __ __ __ __ nn nn)))
  (tiles frame board))

~(multiple-value-bind (frame board)
    (from-tiles
      '((__ __ __ __)
        (__ __ bb __)
        (__ __ __ ww)))
  (tiles frame board))

(defun normalise-board (frame board player)
  "Removes stones of the given player's opponent without liberties."
  (declare (type frame frame)
           (type board board)
           (type colour player))
  (loop :with black = (black board)
        :and white = (white board)
        :with free = (flip (logior black white (edge frame)))
        :with lib = (logior free
                            (black (immortal frame))
                            (white (immortal frame)))
        :with lib-next = lib
        :do
        (setf lib lib-next
              lib-next
              (logand (flip (edge frame))
                      (logior lib
                              (logand +bitmap-left+
                                      (from-left lib)
                                      (logior (from-left free) (logeqv black (from-left black))))
                              (logand +bitmap-right+
                                      (from-right lib)
                                      (logior (from-right free) (logeqv black (from-right black))))
                              (logand +bitmap-top+
                                      (from-top lib)
                                      (logior (from-top free) (logeqv black (from-top black))))
                              (logand +bitmap-bottom+
                                      (from-bottom lib)
                                      (logior (from-bottom free) (logeqv black (from-bottom black)))))))
        :when (= lib lib-next) :do
        (let ((free-next (logior free (logand (flip lib) (logxor white (colour-mask player))))))
          (alterf (slot-value board (opponent player))
                  (curry1 logand (flip free-next) (flip (edge frame))))
          (return))))

(defun vacant-p (board row col)
  (= 0
     (bit-at (black board) row col)
     (bit-at (white board) row col)))

(defun play (frame board player row col)
  "Determines if a move is legal. If so, it also returns the new board."
  (declare (type frame frame)
           (type board board)
           (type colour player)
           (type (integer 0 7) row col))
  (when (vacant-p board row col)
    (let ((pop-before (popcount (slot-value board player)))
          (board (copy-board board)))
      (setf (bit-at (slot-value board player) row col) 1)
      (normalise-board frame board player)
      (normalise-board frame board (opponent player))
      (when (> (popcount (slot-value board player)) pop-before)
        (values t board)))))

(defun plays (frame board moves)
  (match moves
    ((cons (list ?player ?row ?col) ?tail)
     (match (play frame board player row col)
       ((values 't ?new-board)
        (plays frame new-board tail))
       (t nil)))
    (nil (values t board))))

(defun init-and-play (tiles moves)
  (bind (values ?frame ?board)
      (from-tiles tiles)
    (plays frame board moves)))

(defun test-moves (tiles-before moves tiles-after)
  (bind (values ?frame ?board)
      (from-tiles tiles-before)
    (check-match (plays frame board moves)
                 (values 't (of (tiles frame)
                                (the array (equalp (make-tile-array tiles-after)))))
                 (and () (guard (eq tiles-after 'illegal))))))

(deftest test-simple-moves
  (test-moves
    '((__ __)
      (__ __))
    '((black 0 0)
      (white 1 1))
    '((bb __)
      (__ ww))))

(deftest test-capture
  (test-moves
    '((__ bb __)
      (bb ww bb)
      (__ __ __))
    '((black 2 1))
    '((__ bb __)
      (bb __ bb)
      (__ bb __))))

(deftest test-side-capture
  (test-moves
    '((__ bb ww __ __)
      (__ __ bb __ __))
    '((black 0 3))
    '((__ bb __ bb __)
      (__ __ bb __ __))))

(deftest test-corner-capture
  (test-moves
    '((bb ww)
      (__ __))
    '((white 1 0))
    '((__ ww)
      (ww __))))

(deftest test-capture-order
  (test-moves
    '((__ bb bb ww __)
      (bb ww ww __ ww)
      (__ bb bb ww __))
    '((black 1 3))
    '((__ bb bb ww __)
      (bb __ __ bb ww)
      (__ bb bb ww __))))

(deftest test-suicide
  (test-moves
    '((__ bb __)
      (bb __ bb)
      (__ bb __))
    '((white 1 1))
    'illegal))

(deftest test-occupied-own
  (test-moves
    '((__ __)
      (bb __))
    '((black 1 0))
    'illegal))

(deftest test-occupied-opponent
  (test-moves
    '((__ __)
      (bb __))
    '((white 1 0))
    'illegal))

(deftest test-all
  (test-compress-decompress)
  (test-decompress-compress)
  (test-simple-moves)
  (test-capture)
  (test-side-capture)
  (test-corner-capture)
  (test-capture-order)
  (test-suicide)
  (test-occupied-own)
  (test-occupied-own))

~(values-list (collect-test-results #'test-all))

