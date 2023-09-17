(push *default-pathname-defaults* ql:*local-project-directories*)

(ql:quickload :diasbruno)

(setf spinneret:*always-quote* t
      spinneret:*html-style* :human
      spinneret:*html-lang* "en-US"
      local-time:*default-timezone* local-time:+utc-zone+)

(defparameter *posts*
  '(("problems when reexporting javascript modules"
     "problems-when-reexporting-javascript-modules"
     "2023-09-17T22:29:09Z"
     "publish")
    ("working with Sets and some ideas"
     "working-with-sets-and-some-ideas"
     "2023-05-04T23:19:52Z"
     "publish")
    ("zippers"
     "zippers"
     "2023-03-23T12:47:32Z"
     "publish")
    ("even more ideas with types and javascript (equality, ordering and enumeration)"
     "even-more-ideas-with-types-and-javascript-equality-ordering-and-enumeration"
     "2023-03-19T15:35:28Z"
     "publish")
    ("some interesting ideas with types and javascript"
     "some-interesting-ideas-with-types-and-javascript"
     "2023-03-17T21:03:01Z"
     "publish")
    ("forget about state; think about events/commands"
     "forget-about-state-think-about-events-commands"
     "2023-03-12T19:58:55Z"
     "publish")
    ("It's time to retire conventional commit"
     "it-s-time-to-retire-conventional-commit"
     "2023-02-10T14:31:55Z"
     "publish")
    ("Router - more than a web component"
     "router-more-than-a-web-component"
     "2022-07-15T20:09:18Z"
     "hidden")
    ("Specification pattern? Interpreters? Defunctionalization?"
     "specification-pattern-interpreters-defunctionalization"
     "2022-07-02T00:51:21Z"
     "hidden")
    ("Creating a F# project."
     "creating-a-fsharp-project"
     "2022-06-20T23:23:23Z"
     "hidden")
    ("Fun with Rust, MQTT, LoRa and Raspiberry Pi"
     "fun-with-rust-mqtt-lora-and-raspiberry-pi"
     "2022-06-18T19:46:35Z"
     "draft")
    ("Property-based testing for the win!"
     "property-based-testing-for-the-win"
     "2022-05-28T22:30:52Z"
     "draft")
    ("Processes and pipes in OOP"
     "processes-and-pipes-in-oop"
     "2022-03-30T15:00:00Z"
     "draft")
    ("Modeling algorithms with types"
     "modeling-algorithms-with-types"
     "2021-12-03T15:00:00Z"
     "draft")
    ("I love Comby!"
     "i-love-comby"
     "2021-06-08T15:00:00Z"
     "publish")
    ("Retrying promises"
     "retrying-promises"
     "2021-03-23T15:00:00Z"
     "publish")
    ("Transpiling code from other languages"
     "transpiling-code-from-other-languages"
     "2021-03-05T15:00:00Z"
     "publish")
    ("Patterns: Writing a step component"
     "patterns-writing-a-step-component"
     "2020-04-29T15:00:00Z"
     "publish")
    ("I love makefiles"
     "i-love-makefiles"
     "2020-03-21T15:00:00Z"
     "publish")
    ("Metaprogramming in javascript"
     "metaprogramming-in-javascript"
     "2020-03-20T15:00:00Z"
     "publish")))

(defvar *opensource-database*
  (diasbruno.opensource:initialize-opensource-database
   (diasbruno.configuration:opensource-json)))

(defvar *posts-database* nil)
(setf *posts-database*
      (diasbruno.database.post:initialize-post-database *posts*))

