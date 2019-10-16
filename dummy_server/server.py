import flask
from flask_cors import CORS
from flask import jsonify, request

app = flask.Flask(__name__)
CORS(app)


@app.route("/participants/nano")
def participants():
    return flask.send_file("nano.json")


@app.route("/auth/admin/get-token", methods=["POST"])
def login():
    status = (
        request.json.get("username") == "admin"
        and request.json.get("password") == "admin"
    )
    if status:
        return jsonify({"token": "TOKEN"})
    else:
        return "", 401


app.run()
