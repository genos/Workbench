from my_pair import mp_ii
from bottle import Bottle, route, run, template
from subprocess import PIPE, Popen

HOST = "localhost"
PORT = 8080
app = Bottle()

# XXX doesn't quite work
@app.route("/")
def index():
    return "<a href=\"{0}:{1}/17/29\">click here</a>".format(HOST, PORT)

@app.route("/<f:int>/<s:int>")
def pair(f, s):
    return template("Your pair is: {{pair}}", pair=mp_ii(f, s))

@app.route("/test")
def test():
    p = Popen(["ls", "-al"], shell=False, stdout=PIPE)
    return "<pre>{0}</pre>".format(p.communicate()[0])

run(app, host=HOST, port=PORT)
