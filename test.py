import os
import sys
from datetime import date
import argparse


parser = argparse.ArgumentParser()
cwd = os.getcwd()


def write_lines(output_file, lines):
    with open(output_file, "w") as file:
        file.writelines(lines)
    return

def get_lines(input_file):
    with open(input_file, "r") as file:
        lines = file.readlines()
    return lines

def complete_exercise(exercise, exercises):
    exercises[exercise] = ("Y", date.today())
    return

def get_numbering(exercise_str):
    chapter, number = exercise_str.split(".")
    return (int(chapter), int(number))

def generate_markdown(exercises):
    complete = []
    incomplete = []
    for exercise in exercises.keys():
        status = exercises[exercise][0]
        if status == "Y":
            complete.append(exercise)
        else:
            incomplete.append(exercise)
    
    complete.sort(reverse=True, key = get_numbering)
    incomplete.sort(key = get_numbering)
    table_len = max(len(complete), len(incomplete))
    complete_index = 0
    incomplete_index = 0
    lines = []
    lines.append(f"Completed: {len(complete)}|To-do: {len(incomplete)}\n")
    lines.append("|--|--|\n")
    for _ in range(table_len):
        line = "|"
        if complete_index < len(complete):
            e = complete[complete_index]
            line += f"\u2705 {e} {exercises[e][1]}" + "|"
            complete_index += 1
        else:
            line += " |"
        if incomplete_index < len(incomplete):
            line += incomplete[incomplete_index] + "|\n"
            incomplete_index += 1
        else:
            line += " |\n"
        lines.append(line)
    write_lines("test.md", lines)
        

def main():
    input = "exercises_status.txt"
    out_file = "exercises_status.txt"
    exercises = {}
    lines = get_lines(input)
    for line in lines:
        exercise, status, completed_date = line.strip().split("|")
        exercises[exercise] = (status, completed_date)
    for i in range(1, 40):
        if i != 29:
            complete_exercise("1."+str(i), exercises)
    generate_markdown(exercises)
    #write_lines(out_file, lines)
    return

main()