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

(define %dump-directory
  (list-ref (command-line) 2))



(define-dump dump-genewiki-symbols
  (tables (GeneRIF_BASIC
           (left-join Species "USING (SpeciesId)"))
          "GROUP BY GeneId ORDER BY BINARY symbol")
  (schema-triples
   (gn:symbol rdfs:domain gn:geneWikiEntry)
   (gn:wikiEntryOfSpecies rdfs:range gn:species)
   (gn:taxid rdfs:domain gn:geneWikiEntry))
  (triples (ontology 'generif: (field GeneRIF_BASIC GeneId))
    (multiset gn:symbol (string-split (field ("GROUP_CONCAT(DISTINCT symbol)" symbol))
                                      #\,))
    (multiset gn:wikiEntryOfSpecies
              (string-split
               (field ("GROUP_CONCAT(DISTINCT Species.SpeciesName)" species))
               #\,))
    (multiset gn:taxId (map (cut ontology 'taxon: <>)
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
   (gn:geneWikiEntry a rdfs:Class)
   (gn:geneWikiEntry a owl:Class)
   (gn:geneWikiEntry rdfs:comment "Represents GeneRIF Entries")
   (gn:geneCategory rdfs:domain gn:geneWikiEntry)
   (gn:geneWikiEntryOfGn rdfs:domain gn:geneWikiEntry)
   (gn:geneWikiEntry rdfs:domain gn:geneWikiEntry))
  (triples
      (let ([geneid (field GeneRIF_BASIC GeneId)])
        (if (eq? geneid 0)
            (ontology 'gn:anonSymbol_
                      (field GeneRIF symbol))
            (ontology 'generif:
                      geneid)))
    (set rdf:type
         (if (string-null? (field ("IFNULL(GeneRIF_BASIC.GeneId, '')" geneWikiEntryP)))
             ""
             'gn:geneWikiEntry))
    (set gn:wikiEntryOfSpecies
         (field Species SpeciesName))
    ;; This only dumps symbols not present in the GeneRIF_BASIC table
    (set gn:symbol (let ([geneid (field GeneRIF_BASIC GeneId)])
                     (if (eq? geneid 0)
                         (field GeneRIF symbol)
                         "")))
    (multiset gn:geneWikiEntryOfGn
              (let* ([entries
                      (field
                       ("GROUP_CONCAT(DISTINCT CONCAT_WS('::::', IFNULL(GeneCategory.Name, ''), IFNULL(GeneRIF.PubMed_ID, ''), GeneRIF.email, CAST(CONVERT(BINARY CONVERT(GeneRIF.comment USING latin1) USING utf8) AS VARCHAR(15000)), GeneRIF.createtime, IFNULL(weburl, '')) SEPARATOR';;;;;')"
                        wikientry))]
                     [comments (string-split-substring entries ";;;;;")])
                (map
                 (match-lambda
                   ((genecategory pmid email text createtime weburl)
                    (blank-node
                     (set gn:geneCategory genecategory)
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
                     (set gn:geneWikiEntry
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
   (gn:geneWikiEntryofNCBI rdfs:domain gn:geneWikiEntry))
  (triples (ontology 'generif:
                     (field GeneRIF_BASIC GeneId))
    (set gn:geneWikiEntryOfNCBI
         (blank-node
          (set gn:geneWikiEntry
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



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-generif.ttl")
     (lambda ()
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "molecularTrait:" "<http://genenetwork.org/molecular-trait/>")
       (prefix "nuccore:" "<https://www.ncbi.nlm.nih.gov/nuccore/>")
       (prefix "omim:" "<https://www.omim.org/entry/>")
       (prefix "pubchem:" "<https://pubchem.ncbi.nlm.nih.gov/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "hgnc:" "<http://bio2rdf.org/hgnc:>")
       (prefix "homologene:" "<https://bio2rdf.org/homologene:>")
       (prefix "chebi:" "<http://purl.obolibrary.org/obo/CHEBI_>")
       (prefix "kegg:" "<http://bio2rdf.org/ns/kegg#>")
       (newline)
       (dump-genewiki-symbols db)
       (dump-gn-genewiki-entries db)
       (dump-ncbi-genewiki-entries db))
     #:encoding "utf8")))
