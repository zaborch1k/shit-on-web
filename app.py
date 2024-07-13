from flask import Flask, render_template, url_for, request, redirect
from backend.interp import get_data

app = Flask(__name__)


@app.route('/')
def index():
    return render_template('index.html')


@app.route('/json')
def json():
    return render_template('json.html')


@app.route('/run')
def run():
    code = request.args.get('code')
    raw_data = get_data(code)

    # print(raw_data[0])

    err = raw_data[1]
    if not err:
        err = 'null'

    move_data = ' '.join(raw_data[0]).replace("'", '')

    processed_data = {'move-data': move_data, 'err': err}

    # print(processed_data)
    return processed_data


@app.route('/save')
def save():
    print('sdfsddf')
    code = request.args.get('code')
    # print(code)

    # filename = filedialog.asksaveasfilename()

    # if filename:
    #     with open(filename, 'w') as file:
    #         file.write(code)
    return '0'


if __name__ == '__main__':
    app.run(debug=True)
