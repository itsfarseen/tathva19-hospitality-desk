import flask
from flask_cors import CORS
from flask import jsonify, request

app = flask.Flask(__name__)
CORS(app)

participants_data = [
    {
        "_id": "5da1692c9af8ca3218250c79",
        "name": "Barbara",
        "college": "Terrago",
        "email": "barbara@terrago.me",
        "shortid": "incididunt",
        "mobile": 8514972375,
    },
    {
        "_id": "5da1692ce6173034d0ae7687",
        "name": "Mayer",
        "college": "Limage",
        "email": "mayer@limage.biz",
        "shortid": "quis",
        "mobile": 9244763700,
    },
    {
        "_id": "5da1692c4aaa326f031fdd9b",
        "name": "Cooper",
        "college": "Proflex",
        "email": "cooper@proflex.info",
        "shortid": "fugiat",
        "mobile": 8174613229,
    },
    {
        "_id": "5da1692c0b697e3d7c02f946",
        "name": "Patrica",
        "college": "Pyrami",
        "email": "patrica@pyrami.io",
        "shortid": "et",
        "mobile": 9314513911,
    },
    {
        "_id": "5da1692cd959614c198b8fc8",
        "name": "Buck",
        "college": "Comtours",
        "email": "buck@comtours.org",
        "shortid": "nostrud",
        "mobile": 8574122116,
    },
    {
        "_id": "5da1692cf4b2344b433e8533",
        "name": "Baxter",
        "college": "Matrixity",
        "email": "baxter@matrixity.net",
        "shortid": "consequat",
        "mobile": 9994452188,
    },
]


@app.route("/participants")
def participants():
    return jsonify(participants_data)


@app.route("/login")
def login():
    status = (
        request.form.get("userid") == "admin"
        and request.form.get("password") == "admin"
    )
    return jsonify({status: status})


app.run()
