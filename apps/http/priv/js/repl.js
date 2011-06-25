var historyValues = [];
var historyCursor = 0;

function eval_command(command) {
  historyValues.push(command);
  historyCursor = historyValues.length;

  var p = /(\w*) \"([\w/]*)\"( \"(.*)\")?/;
  var res = command.match(p);

  if (res != null) {
    var op = res[1];
    var queue = res[2];
    var msg = res[4];

    if (op == "push") {
      push(queue, msg);
    } else if (op == "pull") {
      pull(queue);
    } else {
      alert("Don't know command " + op);
    }
  }

  $("#input").val('');
}

function append_log(queue, msg) {
  $("#log").append("<div class=\"response\"><span class=\"queue\">" + queue + "</span>: " + msg + "</div>");
}

function escapeHtml(str) {
  str = str.replace(/&/g, "&amp;");
  str = str.replace(/</g, "&lt;");
  str = str.replace(/>/g, "&gt;");
  str = str.replace(/\n/g, "<br>");

  return str;
}

function push(queue, message) {
  $.post(queue, message, function(data) {
    append_log(queue, "pushed message " + message);
  });
}

function pull(queue) {
  $.get(queue, function(data) {
    append_log(queue, "received message " + data[0]);
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