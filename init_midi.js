var data, cmd, channel, type, note, velocity;

let values = {};

// request MIDI access
if (navigator.requestMIDIAccess) {
    navigator.requestMIDIAccess({
        sysex: false
    }).then(onMIDISuccess, onMIDIFailure);
} else {
    alert("No MIDI support in your browser.");
}

// midi functions
function onMIDISuccess(midiAccess) {
    midi = midiAccess;
    var inputs = midi.inputs.values();
    // loop through all inputs
    for (var input = inputs.next(); input && !input.done; input = inputs.next()) {
        // listen for midi messages
 log("input", input);
 log("input.value", input.value);
// listen for midi messages
        input.value.onmidimessage = onMIDIMessage;
        // this just lists our inputs in the console
        listInputs(input);
    }
    // listen for connect/disconnect message
    midi.onstatechange = onStateChange;
}

function onStateChange(event) {
 log("stateChange:", event);
    var port = event.port,
        state = port.state,
        name = port.name,
        type = port.type;
    if (type == "input")  log("name", name, "port", port, "state", state);
}

function onMIDIMessage(event) {
 //log("midims");
    data = event.data,
    values.cmd = data[0] >> 4,
    values.channel = data[0] & 0xf,
    values.type = data[0] & 0xf0, // channel agnostic message type. Thanks, Phil Burk.
    values.note = data[1],
    values.velocity = data[2];
    // cmd15, ch8, type240 is time sync
    // cmd9 is note start, cmd8 is note end
    // cmd11, ch15, type176, note is the ID of the knob, velocity -> 0..127
    // knob IDs: 74, 80-87
 //log(values);
 //log(values.cmd, values.channel, values.type, values.note, values.velocity);
    window.values = values;
}

function listInputs(inputs) {
    var input = inputs.value;
     log("Input port : [ type:'" + input.type + "' id: '" + input.id +
        "' manufacturer: '" + input.manufacturer + "' name: '" + input.name +
        "' version: '" + input.version + "']");
}

function onMIDIFailure(e) {
     log("No access to MIDI devices or your browser doesn't support WebMIDI API. Please use WebMIDIAPIShim " + e);
}

function log(...args) {
  //const elem = document.createElement("pre");
  //elem.textContent = [...args].join(' ');
  //document.body.appendChild(elem);
  console.log(...args);
}
