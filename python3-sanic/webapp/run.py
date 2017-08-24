from sanic import Sanic, response

app = Sanic()

@app.route("/")
async def test(request):
    return response.text('Hello, world! -- Sanic')
