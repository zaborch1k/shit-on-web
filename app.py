from flask import Flask, render_template, url_for, request, redirect

app = Flask(__name__)


@app.route('/')
def index():
    return render_template('index.html')


@app.route('/json')
def json():
    return render_template('json.html')

# background process happening without any refreshing


@app.route('/background_process_test')
def background_process_test():
    code = request.args.get('code') + 'px'
    print(code)
    return (code)


if __name__ == '__main__':
    app.run(debug=True)
