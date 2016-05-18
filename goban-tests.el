(require 'ob-goban)

(ert-deftest goban-test-board-boundaries ()
  "Test a simple board is parsed correctly"
  (let* ((board "
$$B
$$ +----------
$$ | . . 1 . .
$$ | . O . 3 .
$$ | . 2 . . .
$$ | . . . , . ")
         (parsed (goban-parse-board board)))
    (should (goban-board-at-top parsed))
    (should (goban-board-at-left parsed))
    (should (not (goban-board-at-right parsed)))
    (should (not (goban-board-at-bottom parsed)))))


(ert-deftest goban-test-board-boundaries-again ()
  "Test a simple board is parsed correctly"
  (let* ((board "
$$B
$$ . . 1 . . |
$$ . O . 3 . |
$$ . 2 . . . |
$$ . . . , . |
$$ ----------+")
         (parsed (goban-parse-board board)))
    (should (not (goban-board-at-top parsed)))
    (should (not (goban-board-at-left parsed)))
    (should (goban-board-at-right parsed))
    (should (goban-board-at-bottom parsed))))

(ert-deftest goban-test-rows-and-cols-parsed ()
  "Test the correct rows and columns are parsed"
  (let* ((board "
$$B
$$ ------+
$$ . . 1 |
$$ . O 2 |
$$ . X O |
$$ ------+")
         (parsed (goban-parse-board board)))
    (should (equal (goban-board-board parsed)
                   '(("." "." "1")
                     ("." "O" "2")
                     ("." "X" "O"))))))

(ert-deftest goban-test-correct-starting-colour-parsed ()
  "Test the correct starting colour is parsed"
  (let* ((board "
$$B
$$ ------+
$$ . . 1 |
$$ . O 2 |
$$ . X O |
$$ ------+")
         (parsed (goban-parse-board board)))
    (should (equal (goban-board-start-colour parsed)
                   'black))))

(let* ((board "
$$B
$$ . . 1 . . |
$$ . O . 3 . |
$$ . 2 . . . |
$$ . . . , . |
$$ ----------+")
       (parsed (goban-parse-board board)))
  parsed)


