ace.define(
  "ace/mode/qiqe_highlight_rules",
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules =
      require("./text_highlight_rules").TextHighlightRules;

    var QiqeHighlightRules = function () {
      var keywordControl = "if|then|else|let";
      var constantLanguage = "true|false|nil";
      var supportFunctions =
        "print|throw|eval|eq|neq|lt|gt|lte|gte|and|or|not|null";

      var keywordMapper = this.createKeywordMapper(
        {
          "keyword.control": keywordControl,
          "constant.language": constantLanguage,
          "support.function": supportFunctions,
        },
        "identifier",
        true
      );

      this.$rules = {
        start: [
          {
            token: "punctuation.definition.comment",
            regex: "#.*$",
          },
          {
            token: "keyword.operator.qiqe",
            regex: ">>|<<|\\|>|<\\|",
          },
          {
            token: "keyword.operator.function",
            regex: "\\\\|λ|\\.",
          },
          {
            token: "keyword", //parens
            regex: "[\\(|\\)]",
          },
          {
            token: "keyword", //lists
            regex: "[\\[|\\]]",
          },
          {
            token: keywordMapper,
            regex: "[a-zA-Z_][a-zA-Z0-9_']*\\b",
          },
          {
            token: "constant.numeric",
            regex: "\\d+(\\.\\d+)?",
          },
          {
            token: "punctuation.definition.string.begin.qiqe",
            regex: '"',
            push: [
              {
                token: "constant.character.escape.qiqe",
                regex: '\\\\[nrt"]',
              },
              {
                token: "punctuation.definition.string.end.qiqe",
                regex: '"',
                next: "pop",
              },
            ],
          },
        ],
      };
    };

    oop.inherits(QiqeHighlightRules, TextHighlightRules);

    exports.QiqeHighlightRules = QiqeHighlightRules;
  }
);
ace.define(
  "ace/mode/qiqe",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text",
    "ace/mode/qiqe_highlight_rules",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var QiqeHighlightRules =
      require("./qiqe_highlight_rules").QiqeHighlightRules;

    var Mode = function () {
      this.HighlightRules = QiqeHighlightRules;
      this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {
      this.lineCommentStart = "#";
      this.$id = "ace/mode/qiqe";
    }).call(Mode.prototype);

    exports.Mode = Mode;
  }
);
