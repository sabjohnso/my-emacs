;;; my-closet.el --- Where I keep my skeletons -*- lexical-binding: t -*-

(require 'cc-mode)
(require 'json-mode)
(require 'abbrev)

(require 'skeleton)



;;
;; ... C++
;;
(define-skeleton my-c++-main-skeleton
  "Skeleton of a C++ main function"
  nil
  "int" \n
  "main(int" _ ", char**){" \n
  "return 0;" \n
  "}" \n)
(define-abbrev c++-mode-abbrev-table "skmain" "" 'my-c++-main-skeleton)

(define-skeleton my-c++-include-section-skeleton
  "Skeleton for a group of C++ include statements"
  "Name: "
  "//" \n
  "// ... " str " header files" \n
  "//" \n
  "#include <" _ ">")
(define-abbrev c++-mode-abbrev-table "skincs" "" 'my-c++-include-section-skeleton)

(define-skeleton my-c++-include-skeleton
  "Skeleton of an include derective"
  nil
  "#include <" _ ">")
(define-abbrev c++-mode-abbrev-table "skinc" "" 'my-c++-include-skeleton)


(define-skeleton my-c++-namespace-skeleton
  "Skeleton of a C++ namespace"
  "Name: "
  > "namespace " str "{" \n
  > _ \n
  > "} // end of namespace " str \n)
(define-abbrev c++-mode-abbrev-table "skns" "" 'my-c++-namespace-skeleton)


(define-skeleton my-c++-class-skeleton
  "Skeleton of a C++ class"
  "Name: "
  > "/** " \n
  > "* @brief " _ \n
  > " */" \n  
  > "class " str "{" \n
  > "public:" \n
  > "private:" \n
  > "} // end of class " str \n)
(define-abbrev c++-mode-abbrev-table "skcls" "" 'my-c++-namespace-skeleton)


(define-skeleton my-c++-template-parameter-list-skeleton
  "Skeleton of C++ template parametrers"
  ""
  "template<" _ ">")


(define-skeleton my-c++-constructor-skeleton
  "Skeleton of a C++ constructor"
  "Name: "
  str "(" _ "){}")


(define-skeleton my-c++-customization-point-skeleton
  "Skeleton of a C++ customization point"
  "method name: "
  > "namespace details {" \n
  > \n
  > "template<typename T>" \n
  > "void " \n
  > "get_" str "_impl(Type<T>){}" \n
  > \n
  > "struct Get_" (capitalize str) "{" \n
  > "template<typename T>" \n
  > "constexpr auto" \n
  > "operator ()(Type<T>) const {" \n
  > "return get_" str "_impl(type<T>);" \n
  > "}" \n
  > "};" \n
  > \n
  > "} // end of namespace details " \n
  > \n
  > "constexpr auto& get_" str " = details::static_const<details::Get_" (capitalize str) ">;" \n
  > \n
  > "namespace concepts {" \n
  > "template<typename T>" \n
  > "concept Has_" (capitalize str) " = (! std::same_as<void, decltype(get_" str "(type<T>))>);" \n
  > "} // end of namespace concepts" \n
  > "constexpr auto " str " = []<" _ ">(){}" \n
  > \n)
(define-abbrev c++-mode-abbrev-table "skcust" "" 'my-c++-customization-point-skeleton)


;;
;; ... JSON
;;



(define-skeleton my-json-schema-ref-skeleton
  "Skeleton of a JSON schema reference"
  "Name: "
  > "{\"$ref\" : \"#/$defs/" _ "\"}")
(with-eval-after-load 'json-mode
  (define-abbrev json-mode-abbrev-table "skref" "" 'my-json-schema-ref-skeleton))

(define-skeleton my-json-schema-object-skeleton
  "Skeleton of a JSON schema object declaration"
  nil
  > "{" \n
  >   "\"type\" : \"object\"," \n
  >   "\"properties\" : {" \n
  >      _ \n
  >   "}," \n
  >   "\"required\" : []," \n
  >   "\"additionalProperties\" : false" \n
  > "}")
(with-eval-after-load 'json-mode
  (define-abbrev json-mode-abbrev-table "skob" "" 'my-json-schema-object-skeleton))

(define-skeleton my-json-schema-number-skeleton
  "Skeleton of a JSON schema number declaration"
  nil
  "{\"type\" : \"number\"}")
(with-eval-after-load 'json-mode
  (define-abbrev json-mode-abbrev-table "sknum" "" 'my-json-schema-number-skeleton))


(define-skeleton my-json-schema-record-skeleton
  "Skeleton of a JSON schema record type"
  "Name: "
  > "{" \n
  > "\"type\" : \"object\"," \n
  >   "\"properties\" : {" \n
  >       "\"" str "\" : {" \n
  >          "\"type\" : \"object\","  \n
  >          "\"properties\" : {" \n
  >          _ \n
  >           "}," \n
  >       "}," \n
  >       "\"required\" : [\"" str "\"]," \n
  >       "\"additionalProperties\" : false" \n
  >   "}" \n
  > "}")
(with-eval-after-load 'json-mode
  (define-abbrev json-mode-abbrev-table "skrec" "" 'my-json-schema-record-skeleton))


(define-skeleton my-json-schema-schema-skeleton
  "Skeleton of a JSON schema"
  nil
  >  "{" \n
  >     "\"$schema\" : \"https://json-schema.org/draft/2019-09/schema#\"," \n
  >     "\"$id\" : \"" _  "\"" \n
  >  "}")

(with-eval-after-load 'json-mode
  (define-abbrev json-mode-abbrev-table "skschema" "" 'my-json-schema-schema-skeleton))

;; (define-abbrev-table 'json-mode-abbrev-table
;;   '(("skref"    ""    my-json-schema-ref-skeleton)))


;;
;; ... Python
;;

(provide 'my-closet)

