#!/usr/bin/env python

from bottle import (Bottle, abort, error, get, post, redirect, request, run,
    static_file)

app = Bottle()

@app.get("/login")
def login_form():
    return """
<form method="Post" action="/login">
    <input name="name"      type="text" />
    <input name="password"  type="password" />
    <input type="submit" />
</form>
    """

@app.post("/login")
def login_submit():
    name = request.forms.get("name")
    password = request.forms.get("password")
    if check_login(name, password):
        return "<p>Your login was correct</p>"
    else:
        return "<p>Login failed</p>"

@app.route("/static/<filename:path>")
def server_static(filename):
    return static_file(filename, root="/Users/grahamenos/Desktop/workbench",
            mimetype="text")

def check_login(name, password):
    d = {"gre": "123",
         "jlme": "418",
         "other": "pass"}
    return d.get(name, False) == password


@app.error(404)
def error404(error):
    return "Nothing here, sorry!"


@app.route("/restricted")
def restricted():
    abort(401, "Sorry, access denied.")

@app.route("/wrong/url")
def wrong():
    redirect("/")

if __name__ == "__main__":
    run(app, reloader=True, host="localhost", port=8080)
