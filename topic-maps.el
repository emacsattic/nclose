;;
;; The NClosEmacs Project
;;
;; topic-maps.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See: http://www.topicmaps.org/xtm/index.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Investigating Topic Maps
;;
;; At an abstract level, we can say that a topic map encyclopedia consists of a set of addressable information resources, each of which may be located inside of some larger addressable information resource and each of which pertains to one or more subjects. Our subject index consists of the following three things:

;;   1. a set of topics, each of which serves as an electronic surrogate for (reifies) some subject, and each of which may have one or more names
;;   2. links from topics to information resources that are considered to be occurrences of the subjects those topics reify, (e.g. discussed-in, mentioned-in, depicted-in)
;   3. associations between topics, (e.g. example-of, wrote/written-by, lived-in)

;; We use the term topic map to denote any collection of such things. Note that since subjects, as we have defined them, include anything human beings want to think about, discuss, or represent in electronic form, there is no mechanical test to determine whether two subjects are identical or not, or whether two topics reify the same subject or not. Accordingly, the subjects themselves make no appearance in the formal description just given. Nor do we attempt to restrict the nature of the relationships between topics and their occurrences, or between topics and other topics. For this reason, the formalism defined here, while historically developing out of an interest in problems of subject search over bodies of disparate material in many media, may be applied to many problems far distant (or so they appear) from the problems of subject indexing for encyclopedias. The terminology continues to reflect the historical origins of the terms, in the interests of clarity and concreteness.

;;The following is a complete list of XTM element types in the order in which they are documented:
;;    * <topicRef>: Reference to a Topic element
;;    * <subjectIndicatorRef>: Reference to a Subject Indicator
;;    * <scope>: Reference to Topic(s) that comprise the Scope
;;    * <instanceOf>: Points to a Topic representing a class
;;    * <topicMap>: Topic Map document element
;;    * <topic>: Topic element
;;    * <subjectIdentity>: Subject reified by Topic
;;    * <baseName>: Base Name of a Topic
;;    * <baseNameString>: Base Name String container
;;    * <variant>: Alternate forms of Base Name
;;    * <variantName>: Container for Variant Name
;;    * <parameters>: Processing context for Variant
;;    * <association>: Topic Association
;;    * <member>: Member in Topic Association
;;    * <roleSpec>: Points to a Topic serving as an Association Role
;;    * <occurrence>: Resources regarded as an Occurrence
;;    * <resourceRef>: Reference to a Resource
;;    * <resourceData>: Container for Resource data
;;    * <mergeMap>: Merge with another Topic Map
;;

(onto-defstruct tm-topicmap ((:finder . ref)) ref basename instanceOf)

;; Model of an association: a name (ref), and a variable list of topic maps is handled by the onto-defvarstruct macro.

;; In the case of Topic Maps we are confronting an extension of the binary property relation which associates a lname to a rvalue, or a domain to a range.  The concept of an association is much more free-form.  To quote from the specifications: Associations are the general form for the representation of relationships between topics in a topic map. An association can be thought of as an n-ary aggregate of topics. That is, an association is a grouping of topics with no implied direction or order, and there is no restriction on the number of topics that can be grouped together.
;; An association can be assigned a type (again defined by a topic) that specifies the nature of the relationship represented by the association. In addition, each topic that participates in the association plays a typed role that specifies the way in which the topic participates.
;; For example to describe the relationship between a person, "John Smith," and the company he works for, "ABC Limited," we would create an association typed by the topic "Employment" and with role types "Employee" (for the role played by "John Smith") and "Employer" (for the role played by "ABC Limited").
;; Like names, an association can be assigned a scope in which it is valid, and which may be used by a topic map-aware application to determine whether or not to display the information represented by the association to a user in a given situation. 

(onto-defstruct tm-association ((:finder . id)) id roles)

(defmacro onto-defvarstruct (ref &rest roles)
  (let ((spec `(onto-defstruct ,(onto-s-cat "tm-association" ref)
			       ((:finder . id)) id)))
    (loop for role in roles do
	  (setf spec (append spec (list role))))
    `(progn ,spec (tm-association! ',ref ',roles))
    )
)

;; Hence the invocation (onto-defvarstruct Employment Employer Employee) macroexpands into (onto-defstruct tm-association-Employment id Employer Employeee) which in turns makes the required accessors and finders for multirole relationships, e.g. (tm-association-Employment-Employer occurence)
;; It then evaluates to the tm-association object which simply keeps a list of roles in the association.

;; Occurences are simply resources typed by their topic
(onto-defstruct tm-occurence ((:finder . ref)) ref resource type)
