{-# LANGUAGE OverloadedStrings #-}

module Data where

import qualified Data.Map as Map
import Data.Time (defaultTimeLocale, formatTime, parseTimeOrError)
import Data.Time.Clock
import Types

fromString :: String -> UTCTime
fromString = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

posts :: [Post]
posts =
    [ Post
        "problems when reexporting javascript modules"
        "problems-when-reexporting-javascript-modules"
        (fromString "2023-09-17T22:29:09Z")
        Publish
        [ ("title", "og:title", "problems when reexporting javascript modules")
        , ("image", "og:image", "/images/reexport.png")
        ]
    , Post
        "working with Sets and some ideas"
        "working-with-sets-and-some-ideas"
        (fromString "2023-05-04T23:19:52Z")
        Publish
        [ ("title", "og:title", "working with Sets and some ideas")
        , ("image", "og:image", "/images/working-with-sets-and-more-ideas.png")
        ]
    , Post
        "zippers"
        "zippers"
        (fromString "2023-03-23T12:47:32Z")
        Publish
        [ ("title", "og:title", "zippers")
        , ("image", "og:image", "/images/20230323.png")
        ]
    , Post
        "even more ideas with types and javascript (equality, ordering and enumeration)"
        "even-more-ideas-with-types-and-javascript-equality-ordering-and-enumeration"
        (fromString "2023-03-19T15:35:28Z")
        Publish
        [ ("title", "og:title", "even more ideas with types and javascript (equality, ordering and enumeration)")
        , ("image", "og:image", "/images/ideas-with-types.png")
        ]
    , Post
        "some interesting ideas with types and javascript"
        "some-interesting-ideas-with-types-and-javascript"
        (fromString "2023-03-17T21:03:01Z")
        Publish
        [("title", "og:title", "some interesting ideas with types and javascript")]
    , Post
        "forget about state; think about events/commands"
        "forget-about-state-think-about-events-commands"
        (fromString "2023-03-12T19:58:55Z")
        Publish
        [("title", "og:title", "forget about state; think about events/commands")]
    , Post
        "It's time to retire conventional commit"
        "it-s-time-to-retire-conventional-commit"
        (fromString "2023-02-10T14:31:55Z")
        Publish
        [("title", "og:title", "It's time to retire conventional commit")]
    , Post
        "Router - more than a web component"
        "router-more-than-a-web-component"
        (fromString "2022-07-15T20:09:18Z")
        Hidden
        [("title", "og:title", "Router - more than a web component")]
    , Post
        "Specification pattern? Interpreters? Defunctionalization?"
        "specification-pattern-interpreters-defunctionalization"
        (fromString "2022-07-02T00:51:21Z")
        Hidden
        [("title", "og:title", "Specification pattern? Interpreters? Defunctionalization?")]
    , Post
        "Creating a F# project."
        "creating-a-fsharp-project"
        (fromString "2022-06-20T23:23:23Z")
        Hidden
        [("title", "og:title", "Creating a F# project.")]
    , Post
        "Fun with Rust, MQTT, LoRa and Raspiberry Pi"
        "fun-with-rust-mqtt-lora-and-raspiberry-pi"
        (fromString "2022-06-18T19:46:35Z")
        Draft
        [("title", "og:title", "Fun with Rust, MQTT, LoRa and Raspiberry Pi")]
    , Post
        "Property-based testing for the win!"
        "property-based-testing-for-the-win"
        (fromString "2022-05-28T22:30:52Z")
        Draft
        [("title", "og:title", "Property-based testing for the win!")]
    , Post
        "Processes and pipes in OOP"
        "processes-and-pipes-in-oop"
        (fromString "2022-03-30T15:00:00Z")
        Draft
        [("title", "og:title", "Processes and pipes in OOP")]
    , Post
        "Modeling algorithms with types"
        "modeling-algorithms-with-types"
        (fromString "2021-12-03T15:00:00Z")
        Draft
        [("title", "og:title", "Modeling algorithms with types")]
    , Post
        "I love Comby!"
        "i-love-comby"
        (fromString "2021-06-08T15:00:00Z")
        Publish
        [("title", "og:title", "I love Comby!")]
    , Post
        "Retrying promises"
        "retrying-promises"
        (fromString "2021-03-23T15:00:00Z")
        Publish
        [("title", "og:title", "Retrying promises")]
    , Post
        "Transpiling code from other languages"
        "transpiling-code-from-other-languages"
        (fromString "2021-03-05T15:00:00Z")
        Publish
        [("title", "og:title", "Transpiling code from other languages")]
    , Post
        "Patterns: Writing a step component"
        "patterns-writing-a-step-component"
        (fromString "2020-04-29T15:00:00Z")
        Publish
        [("title", "og:title", "Patterns: Writing a step component")]
    , Post
        "I love makefiles"
        "i-love-makefiles"
        (fromString "2020-03-21T15:00:00Z")
        Publish
        [("title", "og:title", "I love makefiles")]
    , Post
        "Metaprogramming in javascript"
        "metaprogramming-in-javascript"
        (fromString "2020-03-20T15:00:00Z")
        Publish
        [("title", "og:title", "Metaprogramming in javascript")]
    ]

oss :: [(String, [Oss])]
oss =
    [
        ( "c"
        , [Oss "slugifier" "https://github.com/diasbruno/slugifier" "c" "a simple slugifier based on unac and pcre2."]
        )
    ,
        ( "common-lisp"
        ,
            [ Oss "cl-bnf" "https://github.com/diasbruno/cl-bnf" "common-lisp" "a bnf parser."
            , Oss "cl-unac" "https://github.com/diasbruno/cl-unac" "common-lisp" "unac(3) bindings for common lisp."
            , Oss "cl-slugify" "https://github.com/diasbruno/cl-slugify" "common-lisp" "convert string into a slug representation. "
            , Oss "domaindsl" "https://github.com/domaindsl/domaindsl" "commonlisp" "A DSL to describe types and generate in your target language."
            ]
        )
    ,
        ( "elixir"
        ,
            [ Oss "email_ex" "https://github.com/diasbruno/email_ex" "elixir" "Address specification parser (RFC2822)."
            ]
        )
    ,
        ( "emacs-lisp"
        ,
            [ Oss "js-eval.el" "https://github.com/diasbruno/js-eval.el" "elisp" "Lets you start a node process in background and start evaluating expressions (like when you are in the scratch buffer in emacs lisp)"
            ]
        )
    ,
        ( "javascript"
        ,
            [ Oss "sbrm" "https://github.com/diasbruno/sbrm" "javascript" "Scoped bound resource management "
            , Oss "instance-provider" "https://github.com/diasbruno/instance-provider" "javascript" "Manage class instances."
            , Oss "bulkjs" "https://github.com/diasbruno/bulkjs" "javascript" "base for a manager for multiple selection."
            , Oss "assoclist.js" "https://github.com/diasbruno/assoclist.js" "javascript" "Basic implementation of lisp's assoc list or haskell's Data.Map.Map."
            , Oss "libfn" "https://github.com/diasbruno/libfn" "javascript" "A micro functional toolbelt (with sharp objects)"
            , Oss "rcwe" "https://github.com/diasbruno/rcwe" "javascript" "React contexts with events."
            , Oss "js-sdk-range" "https://github.com/js-sdk/js-sdk-range" "javascript" "Simple range utilities."
            , Oss "js-sdk-calendar" "https://github.com/js-sdk/js-sdk-calendar" "javascript" "Simple calendar utilities."
            , Oss "js-sdk-timediff" "https://github.com/js-sdk/js-sdk-timediff" "javascript" "Simple timediff utilities."
            , Oss "js-sdk-dict" "https://github.com/js-sdk/js-sdk-dict" "javascript" "Simple dict type."
            , Oss "js-sdk-stack" "https://github.com/js-sdk/js-sdk-stack" "javascript" "Simple stack type."
            , Oss "check-fns" "https://github.com/diasbruno/check-fns" "javascript" "micro data validation."
            , Oss "js-sdk/vdom" "https://github.com/js-sdk/vdom" "javascript" "micro vdom implementation for stable trees."
            , Oss "js-sdk/event-bus" "https://github.com/js-sdk/event-bus" "javascript" "Micro event bus."
            , Oss "js-sdk/router" "https://github.com/js-sdk/router" "javascript" "Router"
            ]
        )
    ,
        ( "haskell"
        ,
            [ Oss "language-js" "https://github.com/diasbruno/language-js" "haskell" "Parser for es6 and es7."
            , Oss "hs-unac-bindings" "https://github.com/diasbruno/hs-unac-bindings" "haskell" "Haskell binding for unac(3)."
            ]
        )
    ,
        ( "ruby"
        ,
            [ Oss "simple-url" "https://github.com/diasbruno/simple-url" "ruby" "A nice way to work with URLs. "
            ]
        )
    ,
        ( "rust"
        ,
            [ Oss "rust_dragino_lgt92_tools" "https://github.com/diasbruno/rust_dragino_lgt92_tools" "rust" "Dragino LGT-92 GPS tools."
            ]
        )
    ]
