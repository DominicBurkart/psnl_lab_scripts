// editable array of targets and option to randomize order they're presented
var targets = ["a parent", "a close friend", "the average American", "a current neighbor"];
var shuffle_targets = true; // iff true, order of targets is randomized for each participant

// variables from PersonProject site
var uid = document.getElementById("user_id").innerHTML;
var sid = document.getElementById("study_id").innerHTML;
var r = document.getElementById('root');

function getCSRFToken() {
    return _.find(document.getElementsByTagName("meta"), (meta) => {
        return meta.name === "csrf-token"
    }).content
}
// end variables from PersonProject site

var ioses = [];
var output = [];


/**
 Inserts the relevant HTML from Desmond Ong's code into root and asynchronously pushes the iOS JS objects to ioses.
 **/
function insert_ios(targets) {
    var add_html = "";
    for (var ti in targets) {
        r.innerHTML += `
         <div id="iOSSlide` + ti + `">
            <center>
                <p id="question` + ti + `">
                     Think about <strong>` + targets[ti] + `</strong>. How close do you feel to this person? Please use the following slider to adjust the circles until they best describe how close you feel:
                </p>
            <div id="iOSDiv` + ti + `" style="height:350px; width:500px; margin: auto;"> </div>
            </center>
        </div>
        `
    }
    window.addEventListener("load", function (e) { // wait until we've got the HTML ready for the JS.
        for (var ti in targets) {
            ioses.push(add_ios(targets[ti], "iOSDiv" + ti));
        }
    });
}

// Fisher-Yates Shuffle
// from https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array
function shuffle(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {

        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}

/**
 * For scoring responses to the continuous integration of the other in the self task.
 * @param arr_of_objs iterable
 * @returns {number} the mean of the values in the iterable (excluding any -999 values)
 */
function score_cois(arr_of_objs) {
    var summed = 0;
    var count = 0;
    for (var o in arr_of_objs) {
        var v = Object.values(arr_of_objs[o])[0]; // there is only one value in each of these
        if (v != -999) {
            summed += v;
            count += 1;
        }
    }
    return summed / count;
}

function color_bg(id){
    document.getElementById(id).style.backgroundColor += "#ffedf8";
}

var warned = false;

function finish() {
    // push data. score == avg closeness to all targets. full data accessible by running
    // afterIOS() on each ios
    output = ioses.map(item => item.afterIOS()); // populates output
    for (var o in output){
        var v = Object.values(output[o])[0]; // there is only one value in each of these
        if (v == -999){
            color_bg("iOSSlide"+o);
            if (!warned) {
                alert("At least one question was left unanswered, which will compromise the interpretation of your results. Please fill in the missing value(s) to get accurate feedback.");
                warned = true;
            }
        }
    }
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
                score: score_cois(output),
                custom_study_results: JSON.stringify(output)
            }
        }
    }).then(returnValue => window.location = returnValue.redirect_url);
    // redirect to results
    console.log({
        user_id: uid,
        study_id: sid,
        score: score_cois(output),
        custom_study_results: JSON.stringify(output)
    });
}

/**
 * from Desmond Ong:
 *
 * https://github.com/desmond-ong/psychWidgets/blob/master/IOS_slider/experiment.js
 */
function mySliderFunction(paper, x1, y1, pathString, colour, pathWidth, iOSCircle1, iOSCircle2, LCircleEdge, RCircleEdge, item_obj) {
    // paper: the Raphael paper object for the entire IOS widget
    // x1: the x location, in this case, of the left circle's left edge
    // y1: the y location
    // pathString: the string that defines the path of the slider. E.g. h200 means horizontal, 200 pixels.
    // color: the color of the slider background
    // pathWidth: the width of the slider

    // iOSCircle1 and 2: the circle objects. They are separate Raphael objects, which will be manipulated/animated by this function
    // LCircleEdge and RCircleEdge: the edges of the L and R circles respectively. This is needed to interpolate the iOSCircles


    //var paper = this;
    var slider = paper.set();
    var position = 0;
    slider.currentValue = 0;
    slider.push(paper.path("M" + x1 + " " + y1 + pathString)).attr({stroke: colour, "stroke-width": pathWidth});
    slider.PathLength = slider[0].getTotalLength();

    initialValue = 0;

    slider.PathPointOne = slider[0].getPointAtLength(position);
    slider.PathPointTwo = slider[0].getPointAtLength(slider.PathLength);
    slider.PathBox = slider[0].getBBox();
    slider.PathBoxWidth = slider.PathBox.width;
    slider.push(paper.circle(slider.PathPointOne.x, slider.PathPointOne.y, pathWidth / 2).attr({
        fill: colour,
        "stroke-width": 0,
        "stroke-opacity": 0
    }));
    slider.push(paper.circle(slider.PathPointTwo.x, slider.PathPointTwo.y, pathWidth / 2).attr({
        fill: colour,
        "stroke-width": 0,
        "stroke-opacity": 0
    }));
    /*Slider Button*/
    // sButtonBack = paper.circle(slider.PathPointOne.x, slider.PathPointOne.y, pathWidth);
    // sButtonBack.attr({fill: "#777", "stroke-width": 1, "fill-opacity": 1, stroke: "#000"});
    // sButtonBack.attr({r: (15)});
    // slider.push(sButtonBack);
    // sliderText = paper.text(slider.PathPointOne.x, slider.PathPointOne.y, initialValue).attr({
    //     fill: '#FFF',
    //     'font-size': 16,
    //     'stroke-width': 0
    // });
    // slider.push(sliderText);
    sButton = paper.circle(slider.PathPointOne.x, slider.PathPointOne.y, pathWidth);
    sButton.attr({fill: "#777", "stroke-width": 1, "fill-opacity": 0.95, stroke: "#000"});
    sButton.attr({r: (15)});

    var start = function () {
        this.ox = this.attr("cx");
    };
    var move = function (dx, dy) {

        var sliderOut = function (currentValue) {
            var scale = (RCircleEdge - LCircleEdge) / 100 / 2;
            iOSCircle1.transform("t" + currentValue * scale + ",0");
            iOSCircle2.transform("t-" + currentValue * scale + ",0");
        };

        item_obj.moved = true;
        pcAlongLine = (this.ox + dx - x1) / slider.PathBoxWidth;
        slider.PathPointOne = slider[0].getPointAtLength(pcAlongLine * slider.PathLength);
        if (!slider.PathPointOne.x) {
            slider.PathPointOne.x = x1;
        }
        if (!slider.PathPointOne.y) {
            slider.PathPointOne.y = y1;
        }
        att = {cx: slider.PathPointOne.x, cy: slider.PathPointOne.y};
        this.attr(att);
        //sButtonBack.attr(att);
        if (Math.round(((this.attr("cx") - slider.PathBox.x) / slider.PathBox.width) * 100)) {
            slider.currentValue = Math.round(((this.attr("cx") - slider.PathBox.x) / slider.PathBox.width) * 100);
        } else {
            slider.currentValue = 0;
        }

        //sliderText.attr({text: slider.currentValue, x: slider.PathPointOne.x, y: slider.PathPointOne.y});
        //bbox = sliderText.getBBox();
        sButton.attr({r: (15)});
        //sButtonBack.attr({r: (15)});
        sliderOut(slider.currentValue);
    };
    up = function () {
    };

    returnValue = function () {
        return currentValue;
    };

    sButton.drag(move, start, up);
    slider.push(sButton);
    return slider;
};


/**
 Quick and dirty modification of Desmond Ong's original continuous ios example.

 When this.afterIOS() is called, the object's response is returned.
 **/
function add_ios(target, target_div) {
    var ios_item = {

        iOS: -1,
        characterName: target,
        moved: false,
        rotateSlider: null,

        showIOS: function () {
            iOSCanvas = Raphael(target_div);
            LCircleEdge = 130;
            RCircleEdge = 350;
            circleYCoord = 125;
            LCircleColor = "#f90";
            RCircleColor = "#09f";
            iOSCircle1 = iOSCanvas.set();
            iOSCircle2 = iOSCanvas.set();

            iOSCircle1.push(iOSCanvas.circle(LCircleEdge, circleYCoord, 100).attr({
                "fill": LCircleColor,
                "stroke-width": 1,
                "fill-opacity": 0.4,
                "stroke": "#000"
            }));
            iOSCircle2.push(iOSCanvas.circle(RCircleEdge, circleYCoord, 100).attr({
                "fill": RCircleColor,
                "stroke-width": 1,
                "fill-opacity": 0.4,
                "stroke": "#000"
            }));
            iOSCircle1.push(iOSCanvas.text(LCircleEdge - 100, circleYCoord - 100, "you").attr({
                "fill": LCircleColor,
                "font-size": 16
            }));
            iOSCircle2.push(iOSCanvas.text(RCircleEdge + 100, circleYCoord - 100, this.characterName).attr({
                "fill": RCircleColor,
                "font-size": 16
            }));
            this.rotateSlider = mySliderFunction(iOSCanvas, LCircleEdge, circleYCoord + 175, 'h200', "#AAAAAA", 15,
                iOSCircle1, iOSCircle2, LCircleEdge, RCircleEdge, this);
        },

        afterIOS: function () {
            if (this.moved) {
                return {[target]: this.rotateSlider.currentValue};
            }
            return {[target]: -999};
        },

    };
    ios_item.showIOS();
    return ios_item;
}

function loadScript(url, callback) {
    // from https://stackoverflow.com/questions/950087/how-do-i-include-a-javascript-file-in-another-javascript-file
    var head = document.getElementsByTagName('head')[0];
    var script = document.createElement('script');
    script.type = 'text/javascript';
    script.src = url;
    script.onreadystatechange = callback;
    script.onload = callback;
    head.appendChild(script);
}


// OK! let's start DOM modification. Load scripts, add HTML and generate JS objects for each item, and run.
loadScript(
    "https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js",
    function () {
        loadScript(
            "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.9.0/underscore-min.js",
            function () {
                loadScript(
                    "https://cdnjs.cloudflare.com/ajax/libs/raphael/2.2.7/raphael.js",
                    function () {
                        // insert the relevant HTML for each IOS scale. Returns the JS object for each item.
                        if (shuffle_targets) {
                            insert_ios(shuffle(targets));
                        }
                        else {
                            insert_ios(targets);
                        }
                        // add finish button. will call finish() which will submit the relevant data and redirect.
                        r.innerHTML += `<button type="button" id="next" onclick="this.blur(); finish()">Next</button>`;
                        //console.log(ioses[0].rotateSlider);
                    }
                )
            }
        )
    }
);
