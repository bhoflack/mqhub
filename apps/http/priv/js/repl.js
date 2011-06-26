var historyValues = [];
var historyCursor = 0;

function eval_command(command) {
  historyValues.push(command);
  historyCursor = historyValues.length;

  var p = /([\w-]*) \"([\w/]*)\"( \"(.*)\")?/;
  var res = command.match(p);

  if (res != null) {
    var op = res[1];
    var queue = res[2];
    var arg = res[4];

    if (op == "push") {
      push(queue, arg);
    } else if (op == "pull") {
      pull(queue);
    } else if (op == "add-listener") {
      add_listener(queue, arg);
    } else if (op == "remove-listener") {
      remove_listener(queue, arg);
    } else {
      alert("Don't know command " + op);
    }
  }

  $("#input").val('');
  scrollDown();
}

function scrollDown() {
  $("#log").attr({ scrollTop: $("#log").attr("scrollHeight") });
};

function append_log(c, r) {
  var template = $("#command-template").html();
  var html = Mustache.to_html(template, {command:c, response:r});
  $("#log").append(html);
}

function escapeHtml(str) {
  str = str.replace(/&/g, "&amp;");
  str = str.replace(/</g, "&lt;");
  str = str.replace(/>/g, "&gt;");
  str = str.replace(/\n/g, "<br>");

  return str;
}

function add_listener(topic, listener) {
  $.post(topic + "/listener", listener, function(data) {
    append_log("add-listener \"" + topic + "\" \"" + listener + "\"", "");
  });
}


function remove_listener(topic, listener) {
  $.ajax(topic + "/listener" + listener,
    { type: 'DELETE',
      success: function(data) {
      append_log("add-listener \"" + topic + "\" \"" + listener + "\"", "");
    }});
}

function push(queue, message) {
  $.post(queue, message, function(data) {
    append_log("push \"" + queue + "\" \"" + message + "\"" , "");
  });
}

function pull(queue) {
  $.get(queue, function(data) {
    append_log("pull \"" + queue + "\"", data[0]);
  }, 'json');
}

function cursorToEnd(input, text) {
  input.val(text);
  setCaretToPos(input.get(0), text.length);
};

function setSelectionRange(input, selectionStart, selectionEnd) {
  if (input.setSelectionRange) {
    input.focus();
    input.setSelectionRange(selectionStart, selectionEnd);
  } else if (input.createTextRange) {
    var range = input.createTextRange();
    range.collapse(true);
    range.moveEnd('character', selectionEnd);
    range.moveStart('character', selectionStart);
    range.select();
  }
};

function setCaretToPos(input, pos) {
  setSelectionRange(input, pos, pos);
};

$(document).ready(function() {
  $("#input").focus();

  $("#input").keydown(function(event) {
    if (event.keyCode == 13) {
      var text = $("#input").val();
      eval_command(text);

      event.preventDefault();
    } else if (event.keyCode == 38) {
      if (historyCursor > 0) {
        var text = historyValues[--historyCursor];
        cursorToEnd($("#input"), escapeHtml(text));
      }
    } else if (event.keyCode == 40) {
      if (historyCursor < historyValues.length - 1) {
        var text = historyValues[++historyCursor];
        cursorToEnd($("#input"), escapeHtml(text));
      } else {
        historyCursor = historyValues.length;
        $("#input").val("");
      }
    }
  });
});