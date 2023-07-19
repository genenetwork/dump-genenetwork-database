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
   (gn-term:name rdfs:range rdfs:Literal))
  (triples (string->identifier "platform" (field GeneChip Name))
    (set rdf:type 'gn:platform)
    (set gn-term:name (field GeneChip GeneChipName))
    (set gn-term:geoPlatform
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
   (gn-term:avgMethod rdfs:range rdfs:Literal)
   (gn-term:dataScale rdfs:range rdfs:Literal)
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
    (set gn-term:avgMethod (string->identifier "avgmethod" (field AvgMethod Name)))
    (set gn-term:fullName (field ProbeSetFreeze FullName))
    (set gn-term:shortName (field ProbeSetFreeze ShortName))
    (set dct:created (annotate-field
                      (field ProbeSetFreeze CreateTime)
                      '^^xsd:datetime))
    (set gn-term:dataScale (field ProbeSetFreeze DataScale))
    (set gn-term:tissueName (string->identifier "tissue" (field Tissue Short_Name)))
    (set gn-term:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))



(dump-with-documentation
 (name "Probeset freeze metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("geoSeries:" "<http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("gn-term:" "<http://genenetwork.org/term/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")))
 (inputs
  (list dump-gene-chip
        dump-probesetfreeze))
 (outputs
  '(#:documentation "./docs/dump-gene-chip.md"
    #:rdf "./verified-data/dump-probesetfreeze.ttl")))
