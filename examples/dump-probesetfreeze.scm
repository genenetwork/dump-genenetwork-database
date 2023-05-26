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


(define-dump dump-gene-chip
  (tables (GeneChip))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
  (triples (string->identifier "platform" (field GeneChip Name))
    (set rdf:type 'gn:platform)
    (set gn:name (field GeneChip GeneChipName))))

;; Molecular Traits are also referred to as ProbeSets
(define-dump dump-probesetfreeze
  (tables (ProbeSetFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = ProbeSetFreeze.Name")
           (left-join ProbeFreeze "USING (ProbeFreezeId)")
           (left-join AvgMethod "ON AvgMethod.AvgMethodId = ProbeSetFreeze.AvgID")
           (left-join InbredSet "ON ProbeFreeze.InbredSetId = InbredSet.Id")
           (left-join Tissue "ON ProbeFreeze.TissueId = Tissue.TissueId"))
          "WHERE ProbeSetFreeze.public > 0 AND InfoFiles.InfoPageName IS NULL GROUP BY ProbeFreeze.Id")
  (schema-triples
   (gn:avgMethod rdfs:range rdfs:Literal)
   (gn:dataScale rdfs:range rdfs:Literal))
  (triples
      (ontology 'probeset:
                (regexp-substitute/global #f "[^A-Za-z0-9:]"
                                          (field ProbeSetFreeze Name)
                                          'pre "_" 'post))
    (set rdf:type 'gn:probesetDataset)
    (set gn:avgMethod (string->identifier "avgmethod" (field AvgMethod Name)))
    (set gn:fullName (field ProbeSetFreeze FullName))
    (set gn:shortName (field ProbeSetFreeze ShortName))
    (set dct:created (annotate-field
                      (field ProbeSetFreeze CreateTime)
                      '^^xsd:datetime))
    (set gn:dataScale (field ProbeSetFreeze DataScale))
    (set gn:tissueName (string->identifier "tissue" (field Tissue Short_Name)))
    (set gn:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-probesetfreeze.ttl")
     (lambda ()
       (prefix "chebi:" "<http://purl.obolibrary.org/obo/CHEBI_>")
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "hgnc:" "<http://bio2rdf.org/hgnc:>")
       (prefix "homologene:" "<https://bio2rdf.org/homologene:>")
       (prefix "kegg:" "<http://bio2rdf.org/ns/kegg#>")
       (prefix "molecularTrait:" "<http://genenetwork.org/molecular-trait/>")
       (prefix "nuccore:" "<https://www.ncbi.nlm.nih.gov/nuccore/>")
       (prefix "omim:" "<https://www.omim.org/entry/>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "pubchem:" "<https://pubchem.ncbi.nlm.nih.gov/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "probeset:" "<http://genenetwork.org/probeset/>")
       (newline)
       (dump-gene-chip db)
       (dump-probesetfreeze db))
     #:encoding "utf8")))
