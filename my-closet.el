;;; my-closet.el --- Where I keep my skeletons -*- lexical-binding: t -*-

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
(define-skeleton my-c++-niebloid-skeleton
  "Skeleton for neibloids"
  "Name: "
  "#pragma once

#include <nstd/details/utils.hpp>

namespace nstd {

  namespace concepts {
    /**
     * @brief A concept for types that implement a nullary member function
     * named `" str "`.
     */
    template<typename T>
    concept Has_" (capitalize str) "_Memfn = requires(T &x) {
      x." str "();
    };
  } // end of namespace concepts

  namespace niebloid {

    template<concepts::Unsatisfiable T>
    void
    " str "_impl(Type<T>) {
    }

    /**
     * @brief A type implementing a method for types that
     * satisfy the `Has_" (capitalize str) "_Memfn` concept.
     */
    struct Execute_" (capitalize str) "_Memfn {
      template<concepts::Has_" (capitalize str) "_Memfn T>
      constexpr decltype(auto)
      operator()(T &&stack) const {
        return std::forward<T>(stack)." str "();
      }
    };

    template<concepts::Has_" (capitalize str) "_Memfn T>
    constexpr Execute_" (capitalize str) "_Memfn
    " str "_impl(Type<T>) {
      return {};
    }


    struct Get_" (capitalize str) " {
      template<typename T>
      requires requires {
        " str "_impl(type<T>);
      }
      constexpr auto
      operator()(Type<T>) const {
        return " str "_impl(type<T>);
      }
    };

  } // end of namespace niebloid

  constexpr auto &get_" str " = details::static_const<details::Get_" (capitalize str) ">;

  namespace concepts {
    template<typename T>
    concept Has_" (capitalize str) " = requires {
      get_" str "(type<T>);
    };

  } // end of namespace concepts

  struct " (capitalize str) " {
    template<typename T>
    constexpr auto
    operator()(T &&stack) const {
      constexpr decltype(auto)  " str " = get_" str "(type<T>);
      return  " str "(std::forward<T>(stack));
    }
  };

  constexpr " (capitalize str) " " str "{};

} // end of namespace nstd")
(define-skeleton my-c++-binary-niebloid-skeleton
  "A skeleton for binary niebloids"
  "Name: "
  "#pragma once

#include <nstd/details/utils.hpp>

namespace nstd {

  namespace concepts {
    /**
     * @brief A concept for types that implement a nullary member function
     * named `" str "`.
     */
    template<typename T, typename Tape>
    concept Has_" (capitalize str) "_Memfn_With = requires(const T &value, Tape &tape) {
      tape." str "(value);
    };
  } // end of namespace concepts

  namespace niebloid {

    template<concepts::Unsatisfiable T>
    void
    " str "_impl(Type<T>) {
    }

    /**
     * @brief A type implementing a method for types that
     * satisfy the `Has_" (capitalize str) "_Memfn` concept.
     */
    struct Execute_" (capitalize str) "_Memfn {
      template<typename T, typename Tape>
      requires concepts::Has_" (capitalize str) "_Memfn_With<T, Tape>
      constexpr decltype(auto)
      operator()(T &&value, Tape &&tape) const {
        return std::forward<Tape>(tape)." str "(std::forward<T>(value));
      }
    };

    template<typename T, typename Tape>
    requires concepts::Has_" (capitalize str) "_Memfn_With<T, Tape>
    constexpr Execute_" (capitalize str) "_Memfn
    " str "_impl(Type<T>, Type<Tape>) {
      return {};
    }

    struct Get_" (capitalize str) " {
      template<typename T, typename Tape>
      requires requires {
        " str "_impl(type<T>, type<Tape>);
      }
      constexpr auto
      operator()(Type<T>, Type<Tape>) const {
        return " str "_impl(type<T>, type<Tape>);
      }
    };

  } // end of namespace niebloid

  constexpr auto &get_" str " = details::static_const<details::Get_" (capitalize str) ">;

  namespace concepts {
    template<typename T, typename Tape>
    concept Has_" (capitalize str) "_With = requires {
      get_" str "(type<T>, type<Tape>);
    };

  } // end of namespace concepts

  struct " (capitalize str) " {
    template<typename T, typename Tape>
    requires concepts::Has_" (capitalize str) "_With<T, Tape>
    constexpr decltype(auto)
    operator()(T &&value, Tape &&tape) const {
      constexpr auto " str " = get_" str "(type<T>, type<Tape>);
      return " str "(std::forward<T>(value), std::forward<Tape>(tape));
    }
  };

  constexpr auto " str " = curry(nat<2>, " (capitalize str) "{});

} // end of namespace nstd
")
(define-skeleton my-c++-include-section-skeleton
  "Skeleton for a group of C++ include statements"
  "Name: "
  "//" \n
  "// ... " str " header files" \n
  "//" \n
  "#include <" _ ">")
(define-skeleton my-c++-include-skeleton
  "Skeleton of an include derective"
  nil
  "#include <" _ ">")
(define-skeleton my-c++-namespace-skeleton
  "Skeleton of a C++ namespace"
  "Name: "
  > "namespace " str "{" \n
  > _ \n
  > "} // end of namespace " str \n)
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
(define-skeleton my-c++-section-skeleton
  "Skeleton of a C++ Catch2 section"
  "Section Name: "
  > "
  SECTION(\"" str "\"){
  }
  ")
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
(with-eval-after-load 'cc-mode
  (define-abbrev c++-mode-abbrev-table "skmain" "" 'my-c++-main-skeleton)
  (define-abbrev c++-mode-abbrev-table "skniebloid" "" 'my-c++-niebloid-skeleton)
  (define-abbrev c++-mode-abbrev-table "skniebloid2" "" 'my-c++-binary-niebloid-skeleton)
  (define-abbrev c++-mode-abbrev-table "skincs" "" 'my-c++-include-section-skeleton)
  (define-abbrev c++-mode-abbrev-table "skinc" "" 'my-c++-include-skeleton)
  (define-abbrev c++-mode-abbrev-table "skns" "" 'my-c++-namespace-skeleton)
  (define-abbrev c++-mode-abbrev-table "skcls" "" 'my-c++-class-skeleton)
  (define-abbrev c++-mode-abbrev-table "sksec" "" 'my-c++-section-skeleton)
  (define-abbrev c++-mode-abbrev-table "skcust" "" 'my-c++-customization-point-skeleton))


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
