#! /usr/bin/env guile
!#

;; This script imports GeneRIF data downloaded from
;; https://ftp.ncbi.nih.gov/gene/GeneRIF/generifs_basic.gz into RDF.

(use-modules (rnrs io ports)
             (srfi srfi-26)
             (srfi srfi-171)
             (ice-9 match)
             (ice-9 regex)
             (dump triples)
             (dump utils)
             (zlib))

(define decode-html-entities
  (cut regexp-substitute/global
       #f
       ;; We tolerate the absence of the trailing semicolon.
       "&#([[:digit:]]+);{0,1}"
       <>
       'pre
       (compose string integer->char string->number (cut match:substring <> 1))
       'post))

(define (main generif-data-file dump-directory)
  (with-output-to-file (string-append dump-directory "/generif.ttl")
    (lambda ()
      (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
      (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
      (prefix "gn:" "<http://genenetwork.org/>")
      (newline)
      ;; TODO: Link to gene objects, not merely literal Gene IDs.
      (triple 'gn:geneId 'rdfs:domain 'gn:geneRIF)
      (triple 'gn:geneId 'rdfs:range 'rdfs:Literal)
      ;; TODO: Link to gn:publication objects, not merely literal
      ;; PubMed IDs.
      (triple 'gn:geneRIFEvidencedByPubMedId 'rdfs:domain 'gn:geneRIF)
      (triple 'gn:geneRIFEvidencedByPubMedId 'rdfs:range 'rdfs:Literal)
      (triple 'gn:geneRIFText 'rdfs:domain 'gn:geneRIF)
      (triple 'gn:geneRIFText 'rdfs:range 'rdfs:Literal)

      (call-with-gzip-input-port (open-input-file generif-data-file)
        (lambda (port)
          ;; Read and discard header.
          (get-line port)
          ;; Dump other lines.
          (port-transduce
           (compose (tenumerate)
                    (tmap (match-lambda
                            ;; Is there a better way to identify
                            ;; GeneRIF entries instead of merely
                            ;; enumerating them?
                            ((i . line)
                             (match (string-split line #\tab)
                               ((_ gene-id pubmed-id _ text)
                                (scm->triples
                                 `((rdf:type . gn:geneRIF)
                                   (gn:geneId . ,(string->number gene-id))
                                   (gn:pubMedId . ,(string->number pubmed-id))
                                   ;; Some text has HTML
                                   ;; entities. Decode them.
                                   (gn:geneRIFText . ,(decode-html-entities text)))
                                 (string->identifier "geneRIF" (number->string i)))))))))
           (const #t)
           get-line
           port))))))

(match (command-line)
  ((arg0 generif-data-file dump-directory)
   (main generif-data-file dump-directory))
  ((arg0 _ ...)
   (format (current-error-port) "Usage: ~a GENERIF-DATA-FILE DUMP-DIRECTORY~%" arg0)
   (exit #f)))
