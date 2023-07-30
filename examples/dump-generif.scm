#! /usr/bin/env guile
!#

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 regex)
             (dump strings)
             (dump sql)
             (dump triples)
             (dump special-forms))



(define %connection-settings
  (call-with-input-file (list-ref (command-line) 1)
    read))



(define-dump dump-genewiki-symbols
  (tables (GeneRIF_BASIC
           (left-join Species "USING (SpeciesId)"))
          "GROUP BY GeneId ORDER BY BINARY symbol")
  (schema-triples
   (gnt:symbol rdfs:domain gn-term:geneWikiEntry)
   (gnt:wikiEntryOfSpecies rdfs:range gn:species)
   (gnt:taxid rdfs:domain gn-term:geneWikiEntry))
  (triples (ontology 'generif: (field GeneRIF_BASIC GeneId))
    (multiset gnt:symbol (string-split (field ("GROUP_CONCAT(DISTINCT symbol)" symbol))
                                      #\,))
    (multiset gnt:wikiEntryOfSpecies
              (string-split
               (field ("GROUP_CONCAT(DISTINCT Species.SpeciesName)" species))
               #\,))
    (multiset gnt:taxId (map (cut ontology 'ncbiTaxon: <>)
                            (string-split (field ("GROUP_CONCAT(DISTINCT TaxID)" taxId))
                                          #\,)))))

(define-dump dump-gn-genewiki-entries
  (tables (GeneRIF
           (left-join GeneRIF_BASIC "USING (symbol)")
           (left-join Species "ON Species.SpeciesId = GeneRIF.SpeciesId")
           (left-join GeneRIFXRef "ON GeneRIFXRef.GeneRIFId = GeneRIF.Id")
           (left-join GeneCategory "ON GeneRIFXRef.GeneCategoryId = GeneCategory.Id"))
          "WHERE GeneRIF.display > 0 AND GeneRIF.VersionId = 0 GROUP BY GeneRIF.symbol")
  (schema-triples
   (gnt:geneWikiEntry a rdfs:Class)
   (gnt:geneWikiEntry a owl:Class)
   (gnt:geneWikiEntry rdfs:comment "Represents GeneRIF Entries")
   (gnt:geneCategory rdfs:domain gn:geneWikiEntry)
   (gnt:geneWikiEntryOfGn rdfs:domain gn:geneWikiEntry)
   (gnt:geneWikiEntry rdfs:domain gn:geneWikiEntry))
  (triples
      (let ([geneid (field GeneRIF_BASIC GeneId)])
        (if (eq? geneid 0)
            (ontology 'gnt:anonSymbol_
                      (field GeneRIF symbol))
            (ontology 'generif:
                      geneid)))
    (set rdf:type
         (if (string-null? (field ("IFNULL(GeneRIF_BASIC.GeneId, '')" geneWikiEntryP)))
             ""
             'gn:geneWikiEntry))
    (set gnt:wikiEntryOfSpecies
         (string->binomial-name (field Species FullName)))
    ;; This only dumps symbols not present in the GeneRIF_BASIC table
    (set gnt:symbol (let ([geneid (field GeneRIF_BASIC GeneId)])
                     (if (eq? geneid 0)
                         (field GeneRIF symbol)
                         "")))
    (multiset gnt:geneWikiEntryOfGn
              (let* ([entries
                      (sanitize-rdf-string
                       (field
                        ("GROUP_CONCAT(DISTINCT CONCAT_WS('::::', IFNULL(GeneCategory.Name, ''), IFNULL(GeneRIF.PubMed_ID, ''), GeneRIF.email, CAST(CONVERT(BINARY CONVERT(GeneRIF.comment USING latin1) USING utf8) AS VARCHAR(15000)), GeneRIF.createtime, IFNULL(weburl, '')) SEPARATOR';;;;;')"
                         wikientry)))]
                     [comments (string-split-substring entries ";;;;;")])
                (map
                 (match-lambda
                   ((genecategory pmid email text createtime weburl)
                    (blank-node
                     (set gnt:geneCategory genecategory)
                     (multiset dct:source
                               (map (lambda (el) (if (string-null? el)
                                                     ""
                                                     (ontology 'pubmed: el)))
                                    (string-split pmid #\space)))
                     (set dct:creator (regexp-substitute/global #f "@.*$"
                                                                email
                                                                'pre
                                                                ""
                                                                'post))
                     (set gnt:geneWikiEntry
                          (annotate-field text '^^xsd:string))
                     (set dct:created (annotate-field
                                       createtime
                                       '^^xsd:datetime))
                     (set foaf:homepage weburl))))
                 (map
                  (cut string-split-substring <> "::::")
                  comments))))))

(define-dump dump-ncbi-genewiki-entries
  (tables (GeneRIF_BASIC)
          "GROUP BY GeneId, comment, createtime")
  (schema-triples
   (gnt:geneWikiEntryofNCBI rdfs:domain gn:geneWikiEntry))
  (triples (ontology 'generif:
                     (field GeneRIF_BASIC GeneId))
    (set gnt:geneWikiEntryOfNCBI
         (blank-node
          (set gnt:geneWikiEntry
               (annotate-field (field GeneRIF_BASIC comment)
                               '^^xsd:string))
          (multiset dct:source (map (lambda (el) (if (string-null? el)
                                                     ""
                                                     (ontology 'pubmed: el)))
                                    (string-split (field ("GROUP_CONCAT(PubMed_ID)" pmids))
                                                  #\,)))
          (set dct:created (annotate-field (time-unix->string
                                            (field GeneRIF_BASIC createtime) "~5")
                                           '^^xsd:datetime))))))



(dump-with-documentation
 (name "GeneRIF Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("dct:" "<http://purl.org/dc/terms/>")
    ("pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
    ("ncbiTaxon:" "<https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=>")
    ("generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")
    ("owl:" "<http://www.w3.org/2002/07/owl#>")))
 (inputs
  (list ;; dump-genewiki-symbols
        dump-gn-genewiki-entries
        ;; dump-ncbi-genewiki-entries
        ))
 (outputs
   '(#:documentation "./docs/dump-generif.md"
     #:rdf "./verified-data/dump-generif.ttl")))
