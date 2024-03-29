; A Dice of Doom game, from the Land of Lisp book by Conrad Barski
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

; Convert a board's list representation to an array
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

; Create a random board
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

; Convert a player number to a letter
(defun player-letter (n)
  (code-char (+ 97 n)))


; Draw the board on the console
(defun draw-board (board)
  (loop for y below *board-size* do
        (progn (fresh-line)
               (loop repeat (- *board-size* y) do (princ " "))
               (loop for x below *board-size*
                     for hex = (aref board (+ x (* *board-size* y)))
                     do (format t "~a-~a "
                                (player-letter (first hex))
                                (second hex))))))

; Master rule-set function
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

; Passing moves to add to a game tree
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (cons (list nil
                (game-tree (add-new-dice board player (1- spare-dice))
                           (mod (1+ player) *num-players*)
                           0
                           t))
          moves)))

; Attacking moves to add to the game tree
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos) (car (aref board pos)))
           (dice (pos) (cadr (aref board pos))))
    (mapcan
      (lambda (src)
        (when (eq (player src) cur-player)
          (mapcan
            (lambda (dst)
              (when (and (not (eq (player dst) cur-player))
                         (> (dice src) (dice dst)))
                (list
                  (list
                    (list src dst)
                    (game-tree (board-attack board cur-player src dst (dice src))
                               cur-player
                               (+ spare-dice (dice dst))
                               nil)))))
            (neighbours src))))
      (loop for n below *board-hexnum* collect n))))

; Gets the neighbouring hexes
(defun neighbours (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))

; Figures out what happens when hex src attacks hex dst
(defun board-attack (board player src dst dice)
  (board-array (loop for pos
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

; Calculates reinforcement dice
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
              (cond ((null lst) nil)
                    ((zerop n) lst)
                    (t
                      (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                          (cons (list cur-player (1+ cur-dice))
                                (f (cdr lst) (1- n)))
                          (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

; 2-player version
(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

; Print out some game info
(defun print-info (tree)
  (fresh-line)
  (format t "Current player: ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

; Let the human pick a move
(defun handle-human (tree)
  (fresh-line)
  (princ "Choose your move...:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                 (format t "~a -> ~a" (car action) (cadr action))
                 (princ "end turn."))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

; Determine the winners of a game
(defun winners (board)
  (let* ((tally (loop for hex across board collect (car hex)))
         (totals (mapcar (lambda (player) (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x) (not (eq (cdr x) best))) totals))))

; Print the winner
(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a draw between ~a" (mapcar #'player-letter w))
      (format t "The winner is: ~a" (player-letter (car w))))))

; AI - rate a move
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
      (apply (if (eq (car tree) player) #'max #'min)
             (get-ratings tree player))
      (let ((w (winners (cadr tree))))
        (if (member player w)
          (/ 1 (length w))
          0)))))

; AI - get ratings for moves in a game tree
(defun get-ratings (tree player)
  (mapcar (lambda (move) (rate-position (cadr move) player))
          (caddr tree)))

; AI - the computer player
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

; Human vs computer
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

; Memoize the neighbours function
(let ((old-neighbours (symbol-function 'neighbours))
      (previous (make-hash-table)))
  (defun neighbours (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbours pos)))))

; Memoize the game-tree function
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rst)
    (or (gethash rst previous)
        (setf (gethash rst previous) (apply old-game-tree rst)))))

; Memoize rate-position
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab) (funcall old-rate-position tree player))))))

; Tail-call optimized add-new-dice
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
              (cond ((zerop n) (append (reverse acc) lst))
                    ((null lst) (reverse acc))
                    (t (let ((cur-player (caar lst))
                             (cur-dice (cadar lst)))
                         (if (and (eq cur-player player)
                                  (< cur-dice *max-dice*))
                           (f (cdr lst)
                              (1- n)
                              (cons (list cur-player (1+ cur-dice)) acc))
                           (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))
