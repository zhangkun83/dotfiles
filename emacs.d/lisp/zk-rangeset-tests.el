;;; zk-rangeset-tests.el --- Tests for zk-rangeset.el -*- lexical-binding: t; -*-

(require 'ert)
(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name default-directory))))
(require 'zk-rangeset)

(ert-deftest zk-rangeset-test-empty ()
  (should (equal (zk-rangeset-add nil '(1 . 2))
                 '((1 . 2)))))

(ert-deftest zk-rangeset-test-single-range-relations ()
  "Test all possible relations between a new range and a single existing range [5, 10)."
  (let ((rs '((5 . 10))))
    ;; Case 1: New range is strictly before, no touch (e < b1)
    (should (equal (zk-rangeset-add rs '(1 . 3))
                   '((1 . 3) (5 . 10))))
    (should (equal (zk-rangeset-add rs '(1 . 4))
                   '((1 . 4) (5 . 10))))

    ;; Case 2: New range touches on the left (e == b1)
    (should (equal (zk-rangeset-add rs '(1 . 5))
                   '((1 . 10))))

    ;; Case 3: Overlap on the left (b < b1 < e < e1)
    (should (equal (zk-rangeset-add rs '(3 . 7))
                   '((3 . 10))))

    ;; Case 4: Same start, new is subset (b == b1, e < e1)
    (should (equal (zk-rangeset-add rs '(5 . 8))
                   '((5 . 10))))

    ;; Case 5: New is strict subset (b1 < b, e < e1)
    (should (equal (zk-rangeset-add rs '(6 . 8))
                   '((5 . 10))))

    ;; Case 6: New is subset, same end (b1 < b, e == e1)
    (should (equal (zk-rangeset-add rs '(7 . 10))
                   '((5 . 10))))

    ;; Case 7: Identical (b == b1, e == e1)
    (should (equal (zk-rangeset-add rs '(5 . 10))
                   '((5 . 10))))

    ;; Case 8: Same start, new is superset (b == b1, e > e1)
    (should (equal (zk-rangeset-add rs '(5 . 12))
                   '((5 . 12))))

    ;; Case 9: Overlap on the right (b1 < b < e1 < e)
    (should (equal (zk-rangeset-add rs '(7 . 12))
                   '((5 . 12))))

    ;; Case 10: New range touches on the right (e1 == b)
    (should (equal (zk-rangeset-add rs '(10 . 12))
                   '((5 . 12))))

    ;; Case 11: New range is strictly after, no touch (e1 < b)
    (should (equal (zk-rangeset-add rs '(12 . 15))
                   '((5 . 10) (12 . 15))))
    (should (equal (zk-rangeset-add rs '(11 . 15))
                   '((5 . 10) (11 . 15))))

    ;; Case 12: New is strict superset (b < b1, e > e1)
    (should (equal (zk-rangeset-add rs '(3 . 12))
                   '((3 . 12))))))

(ert-deftest zk-rangeset-test-multiple-ranges-relations ()
  "Test relations involving multiple existing ranges."
  ;; Base rangeset: [1, 3) and [6, 8)
  (let ((rs '((1 . 3) (6 . 8))))
    ;; Insert between, no touch
    (should (equal (zk-rangeset-add rs '(4 . 5))
                   '((1 . 3) (4 . 5) (6 . 8))))

    ;; Touch left only (touches [1, 3) on right)
    (should (equal (zk-rangeset-add rs '(3 . 4))
                   '((1 . 4) (6 . 8))))

    ;; Touch right only (touches [6, 8) on left)
    (should (equal (zk-rangeset-add rs '(5 . 6))
                   '((1 . 3) (5 . 8))))

    ;; Bridge both (touches both: e1==b, e==b2)
    (should (equal (zk-rangeset-add rs '(3 . 6))
                   '((1 . 8))))

    ;; Bridge both (overlaps both: b<e1, e>b2)
    (should (equal (zk-rangeset-add rs '(2 . 7))
                   '((1 . 8))))

    ;; Bridge both (superset of both)
    (should (equal (zk-rangeset-add rs '(0 . 9))
                   '((0 . 9))))))

(ert-deftest zk-rangeset-test-large-merge ()
  "Test merging multiple ranges in a larger set."
  (let ((rs '((1 . 3) (5 . 7) (9 . 11) (13 . 15) (17 . 19))))
    ;; Merge middle three ranges
    (should (equal (zk-rangeset-add rs '(6 . 14))
                   '((1 . 3) (5 . 15) (17 . 19))))
    ;; Merge all
    (should (equal (zk-rangeset-add rs '(0 . 20))
                   '((0 . 20))))
    ;; Merge none, insert at beginning (no touch)
    (should (equal (zk-rangeset-add rs '(-1 . 0))
                   '((-1 . 0) (1 . 3) (5 . 7) (9 . 11) (13 . 15) (17 . 19))))
    ;; Merge none, insert at end
    (should (equal (zk-rangeset-add rs '(20 . 21))
                   '((1 . 3) (5 . 7) (9 . 11) (13 . 15) (17 . 19) (20 . 21))))))

(ert-deftest zk-rangeset-test-invalid-or-empty-range ()
  "Test adding invalid or empty ranges (begin >= end)."
  (let ((rs '((1 . 3) (5 . 7))))
    ;; Adding empty range (b == e)
    (should (equal (zk-rangeset-add rs '(4 . 4))
                   rs))
    (should (equal (zk-rangeset-add rs '(6 . 6))
                   rs))
    ;; Adding invalid range (b > e)
    (should (equal (zk-rangeset-add rs '(4 . 2))
                   rs))
    ;; Adding to empty rangeset
    (should (equal (zk-rangeset-add nil '(4 . 4))
                   nil))))

(ert-deftest zk-rangeset-test-non-destructive ()
  "Verify that the original rangeset structure is not mutated."
  (let* ((rs '((5 . 10) (15 . 20)))
         (rs-copy (copy-sequence rs)))
    (zk-rangeset-add rs '(12 . 14))
    (should (equal rs rs-copy))
    (zk-rangeset-add rs '(8 . 12))
    (should (equal rs rs-copy))
    (zk-rangeset-add rs '(3 . 25))
    (should (equal rs rs-copy))))

(ert-deftest zk-rangeset-test-negative-numbers ()
  "Test rangeset operations with negative numbers."
  (let ((rs '((-10 . -5) (-2 . 3))))
    (should (equal (zk-rangeset-add rs '(-5 . -2))
                   '((-10 . 3))))
    (should (equal (zk-rangeset-add rs '(-15 . -12))
                   '((-15 . -12) (-10 . -5) (-2 . 3))))
    (should (equal (zk-rangeset-add rs '(-4 . -3))
                   '((-10 . -5) (-4 . -3) (-2 . 3))))))

(ert-deftest zk-rangeset-test-sequential-builder ()
  "Test building a rangeset sequentially."
  (let ((rs nil))
    (setq rs (zk-rangeset-add rs '(10 . 20)))
    (should (equal rs '((10 . 20))))
    (setq rs (zk-rangeset-add rs '(30 . 40)))
    (should (equal rs '((10 . 20) (30 . 40))))
    (setq rs (zk-rangeset-add rs '(0 . 5)))
    (should (equal rs '((0 . 5) (10 . 20) (30 . 40))))
    (setq rs (zk-rangeset-add rs '(15 . 25)))
    (should (equal rs '((0 . 5) (10 . 25) (30 . 40))))
    (setq rs (zk-rangeset-add rs '(26 . 35)))
    (should (equal rs '((0 . 5) (10 . 25) (26 . 40))))
    (setq rs (zk-rangeset-add rs '(25 . 26)))
    (should (equal rs '((0 . 5) (10 . 40))))))

(ert-deftest zk-rangeset-test-large-scale ()
  "Test with a large number of ranges to ensure no stack overflow and correct behavior."
  (let ((rs nil)
        (n 5000))
    ;; Create a rangeset with N ranges: ((0 . 1) (2 . 3) (4 . 5) ... (2N-2 . 2N-1))
    (dotimes (i n)
      (setq rs (cons (cons (* 2 i) (1+ (* 2 i))) rs)))
    (setq rs (nreverse rs))
    ;; rs is now sorted.

    ;; Add a range at the very end.
    (let ((new-rs (zk-rangeset-add rs (cons (* 2 n) (1+ (* 2 n))))))
      (should (= (length new-rs) (1+ n))))

    ;; Add a range that merges everything.
    (let ((new-rs (zk-rangeset-add rs (cons -1 (* 2 n)))))
      (should (equal new-rs (list (cons -1 (* 2 n))))))))

(ert-deftest zk-rangeset-test-bigints ()
  "Test rangeset operations with large integers (bignums)."
  (let ((rs '((100000000000000000000000000000 . 100000000000000000000000000010))))
    (should (equal (zk-rangeset-add rs '(100000000000000000000000000005 . 100000000000000000000000000020))
                   '((100000000000000000000000000000 . 100000000000000000000000000020))))
    (should (equal (zk-rangeset-add rs '(100000000000000000000000000015 . 100000000000000000000000000020))
                   '((100000000000000000000000000000 . 100000000000000000000000000010)
                     (100000000000000000000000000015 . 100000000000000000000000000020))))))



