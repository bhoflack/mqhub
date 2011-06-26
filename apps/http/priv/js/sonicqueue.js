var SonicQueue = function() {

  return({
    name: "sonicqueue.js",
    version: "1.0.0",

    /*
     * Push a message to a queue/topic.
     *
     * Call the callback with the result.
     */
    push: function(queue, message, cb) {
      $.post(queue, message, cb);
    },

    /*
     * Pull a message from a queue.
     *
     * Call the callback with the result.
     */
    pull: function(queue, cb) {
      $.get(queue, cb, 'json');
    },

    /*
     * Add a listener to a topic.
     *
     * topic: a valid topic
     * listener: a queue
     * cb: a callback function to be called when executed.
     */
    add_listener: function(topic, listener, cb) {
      $.post(topic + "/listener", listener,  cb);
    },

    /*
     * Remove a listener from a topic.
     *
     * topic: a valid topic
     * listener: a queue
     * cb a callback function to be called when executed.
     */
    remove_listener: function(topic, listener, cb) {
      $.ajax(topic + "/listener" + listener,
        { type: 'DELETE',
          success: cb
        });
    }
  });
}();