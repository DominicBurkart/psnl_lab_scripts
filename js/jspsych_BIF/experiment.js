var uid = document.getElementById("user_id").innerHTML
var sid = document.getElementById("study_id").innerHTML

function loadScript(url, callback){
    // from https://stackoverflow.com/questions/950087/how-do-i-include-a-javascript-file-in-another-javascript-file
    var head = document.getElementsByTagName('head')[0];
    var script = document.createElement('script');
    script.type = 'text/javascript';
    script.src = url;
    script.onreadystatechange = callback;
    script.onload = callback;
    head.appendChild(script);
}


var intention = [
  "Getting organized",
  "Gaining knowledge",
  "Helping the nation's defense",
  "Removing odors from clothes",
  "Getting something to eat",
  "Getting firewood",
  "Getting ready to remodel",
  "Showing one's cleanliness",
  "Making the room look fresh",
  "Maintaining a place to live",
  "Making the room look nice",
  "Securing the house",
  "Influencing the election",
  "Getting a good view",
  "Revealing what you're like",
  "Preventing tooth decay",
  "Showing one's knowledge",
  "Showing friendliness",
  "Showing moral courage",
  "Getting nutrition",
  "Getting fresh vegetables",
  "Seeing countryside",
  "Protecting your teeth",
  "Teaching a child something",
  "Seeing if someone's home"
]

var action = [
  "Writing things down",
  "Following lines of print",
  "Signing up",
  "Putting clothes into the machine",
  "Pulling an apple off a branch",
  "Wielding an axe",
  "Using a yard stick",
  "Vacuuming the floor",
  "Applying brush strokes",
  "Writing a check",
  "Watering plants",
  "Putting a key in the lock",
  "Marking a ballot",
  "Holding on to branches",
  "Answering questions",
  "Moving a brush around in one's mouth",
  "Answering questions",
  "Saying hello",
  "Saying \"no\"",
  "Chewing and swallowing",
  "Planting seeds",
  "Following a map",
  "Going to the dentist",
  "Using simple words",
  "Moving a finger"
]

var count = 0
function bytwo(){
  var r = Math.floor(count / 2);
  count = count + 1;
  return r
}

function score_bif(rstring){
  var responses = JSON.parse(rstring)
  var score = 0;
  for (var q in responses){
    if (intention.includes(responses[q])){
      score = score + 1;
    }
  }
  return score
}

var experiment = function() {

  var p1 = {
    type: 'survey-multi-choice',
    questions: [
      { prompt: "Any behavior can be described in many ways. For example, one person might describe a behavior as \"writing a paper,\" while another person might describe the same behavior as \"pushing keys on the keyboard.\" Yet another person might describe it as \"expressing thoughts.\" This form focuses on your personal preferences for how a number of different behaviors should be described. Below you will find several behaviors listed. After each behavior will be two different ways in which the behavior might be identified.",
      options:[]
      },
      {
        prompt: "Making a list",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Reading",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Joining the army",
        options: [intention[bytwo()], action[bytwo()]], //  0
        required: true
      },
      {
        prompt: "Washing clothes",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Picking an apple",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Chopping down a tree",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Measuring a room for carpeting",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Cleaning the house",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Painting a room",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Paying the rent",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Caring for houseplants",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Locking a door",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Voting",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Climbing a tree",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Filling out a personality test",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Toothbrushing",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Taking a test",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Greeting someone",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Resisting temptation",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Eating",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Growing a garden",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Traveling by car",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      },
      {
        prompt: "Having a cavity filled",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Talking to a child",
        options: [intention[bytwo()], action[bytwo()]], // 0
        required: true
      },
      {
        prompt: "Pushing a doorbell",
        options: [action[bytwo()], intention[bytwo()]], // 1
        required: true
      }
    ]
  }

  function getCSRFToken() {
       return _.find(document.getElementsByTagName("meta"), (meta) => {
         return meta.name === "csrf-token"
       }).content
   }

  jsPsych.init({
      display_element: "root",
      timeline: [p1],
      on_finish: function(){
        var d = jsPsych.data.get();
        // console.log(d);
        // console.log(d.values());
        var parsed = JSON.parse(d.values()['0'].responses);
        // console.log(parsed);
        var lcust = []
        for (var k in parsed) {
          lcust.push({[k]: parsed[k]})
        }
        // console.log(JSON.stringify(lcust));

        $.post({
          url: "/studies.json",
          headers: {
            'X-CSRF-Token': getCSRFToken()
          },
          dataType: 'json',
          data: {
            study: {
              user_id: uid,
              study_id: sid,
              custom_study_results: JSON.stringify(lcust),
              score: score_bif(d.values()[0].responses)
            }
          }
        }).then(returnValue => window.location = returnValue.redirect_url)
      }
  });
}


loadScript(
  "https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js",
  function(){
    loadScript(
      "https://s3.us-east-2.amazonaws.com/personproject/shared_experiment_scripts/underscore-min.js",
      function () {
        loadScript(
        "https://s3.us-east-2.amazonaws.com/personproject/shared_experiment_scripts/jspsych.js",
          function (){
            loadScript(
              "https://s3.us-east-2.amazonaws.com/personproject/shared_experiment_scripts/jspsych-survey-multi-choice.js",
              experiment
            )
          }
        )
      }
    )
  }
)
