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


(define-dump dump-gene-chip
  (tables (GeneChip))
  (schema-triples
   (gnt:name rdfs:range rdfs:Literal))
  (triples (string->identifier "platform" (field GeneChip Name))
    (set rdf:type 'gn:platform)
    (set gnt:name (field GeneChip GeneChipName))
    (set gnt:geoPlatform
         (ontology 'geoSeries:
                   (string-trim-both (field GeneChip GeoPlatform))))))

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
   (gnt:avgMethod rdfs:range rdfs:Literal)
   (gnt:dataScale rdfs:range rdfs:Literal)
   (gn:probesetDataset rdf:subClassOf gn:dataset))
  (triples
      (string->identifier
       ""
       (regexp-substitute/global
        #f "[^A-Za-z0-9:]"
        (field ProbeSetFreeze Name)
        'pre "_" 'post)
       #:separator ""
       #:proc string-capitalize-first)
    (set rdf:type 'gn:probesetDataset)
    (set gnt:avgMethod (string->identifier "avgmethod" (field AvgMethod Name)))
    (set gnt:fullName (field ProbeSetFreeze FullName))
    (set gnt:shortName (field ProbeSetFreeze ShortName))
    (set dct:created (annotate-field
                      (field ProbeSetFreeze CreateTime)
                      '^^xsd:datetime))
    (set gnt:dataScale (field ProbeSetFreeze DataScale))
    (set gnt:tissueName (string->identifier "tissue" (field Tissue Short_Name)))
    (set gnt:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))



(dump-with-documentation
 (name "Probeset freeze metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("dct:" "<>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")))
 (inputs
  (list dump-gene-chip
        dump-probesetfreeze))
 (outputs
  '(#:documentation "./docs/dump-gene-chip.md"
    #:rdf "./verified-data/dump-probesetfreeze.ttl")))
