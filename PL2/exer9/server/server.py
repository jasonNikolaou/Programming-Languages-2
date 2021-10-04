from flask import Flask, redirect, url_for, render_template, request, jsonify, session
import random
import math


# ==== global variables ======
primes = [47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
totalQuestions = 5
N_max = 100

# ==== help functions =====
def calcSol(n, k, p):
    return math.comb(n, k) % p

def newQuestion():
    n = random.randint(0, N_max)
    k = random.randint(0, n)
    p = primes[random.randint(0, len(primes)-1)]
    return n, k, p
# build server
app = Flask(__name__)
app.secret_key = 'secret_key'

@app.route('/', methods = ['POST', 'GET'])
def question():
    if 'n' not in session:
        session['n'], session['k'], session['p'] = newQuestion()
        session['questionNum'] = 1
        session['numOfCorrectAns'] = 0
    if request.method == 'POST':
        answer = request.form['answer']
        try:
            if int(answer) == calcSol(session['n'], session['k'], session['p']):
                return redirect(url_for('answer', correct=1))
            else:
                return redirect(url_for('answer', correct=0))
        except Exception as e:
            return redirect(url_for('answer', correct=0))
    return render_template('question.html',
                            n=session['n'], k=session['k'], p=session['p'],
                            questionNum=session['questionNum'],
                            totalQuestions=totalQuestions)

@app.route('/answer/<int:correct>')
def answer(correct):
    # new question
    session['n'], session['k'], session['p'] = newQuestion()
    session['questionNum'] += 1

    if correct:
        session['numOfCorrectAns'] += 1

    if session['questionNum'] > totalQuestions:
        prevCorrect = session['numOfCorrectAns']
        session['questionNum'] = 1
        session['numOfCorrectAns'] = 0
        if correct:
            return render_template('end.html', numOfCorrectAns=prevCorrect,
                                            totalQuestions=totalQuestions,
                                            message="Correct")
        else:
            return render_template('end.html', numOfCorrectAns=prevCorrect,
                                            totalQuestions=totalQuestions,
                                            message="Wrong")

    if correct:
        return render_template('next.html', message="Correct")
    else:
        return render_template('next.html', message="Wrong")

if __name__ == '__main__':
    app.run()
