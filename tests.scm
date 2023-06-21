(use-modules (srfi srfi-64)
             (ice-9 match)
             (dump sql)
             (dump string-similarity)
             (dump strings)
             (dump special-forms))

(test-begin "sql")

(test-equal "select query"
  "SELECT InfoFiles.GN_AccesionId, InfoFiles.InfoFileTitle AS Name, InfoFiles.Title, InfoFiles.Specifics, DatasetStatus.DatasetStatusName FROM InfoFiles LEFT JOIN DatasetStatus USING (DatasetStatusId) WHERE GN_AccesionId IS NOT NULL"
  (select-query ((InfoFiles GN_AccesionId)
                 (InfoFiles InfoFileTitle Name)
                 (InfoFiles Title)
                 (InfoFiles Specifics)
                 (DatasetStatus DatasetStatusName))
                (InfoFiles
                 (left-join DatasetStatus "USING (DatasetStatusId)"))
                "WHERE GN_AccesionId IS NOT NULL"))

(test-end "sql")

(test-begin "string-similarity")

(test-equal "trigrams of a string"
  (list "coe" "oef" "eff" "ffi" "fic" "ici" "cie" "ien" "ent")
  ((@@ (dump string-similarity) trigrams) "coefficient"))

(test-equal "Jaccard index"
  0.4
  ((@@ (dump string-similarity) jaccard-index)
   (list 0 1 2 5 6 8 9)
   (list 0 2 3 4 5 7 9)))

(test-equal "Jaccard index of equal sets"
  1.0
  ((@@ (dump string-similarity) jaccard-index)
   (iota 10)
   (iota 10)))

(test-equal "Jaccard index of disjoint sets"
  0.0
  ((@@ (dump string-similarity) jaccard-index)
   (iota 10)
   (iota 10 10)))

(test-end "string-similarity")

(test-begin "utils")

(test-equal "map-alist docstring example"
  '((spam . 2)
    (ham . 4)
    (eggs . 6)
    (aal . 3)
    (vel . 0)
    (vel . 1)
    (vel . 2)
    (vel . 3)
    (fubar . 8))
  (map-alist '(("foo" . 1)
               (bar . 2)
               (foobar . 5)
               (fubar . 3))
    (set spam (1+ (key "foo")))
    (set ham (* 2 (key bar)))
    (set eggs (* 3 (key bar)))
    (set aal (+ (key "foo")
                (key bar)))
    (multiset vel (iota (* 2 (key bar))))
    (remove foobar)
    (else=> (match-lambda
              ((key . value)
               (cons key (expt 2 value)))))))

(test-equal "multiple set actions in map-alist should result in multiple keys"
  '((spam . 4)
    (spam . 6))
  (map-alist '((foo . 2))
    (set spam (* 2 (key foo)))
    (set spam (* 3 (key foo)))))

(test-end "utils")
