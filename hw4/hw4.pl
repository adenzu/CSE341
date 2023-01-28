% knowledge base

% part 1

% room_capacity(ROOM_ID, CAPACITY)
room_capacity(0, 120).
room_capacity(1, 75).

% room_equipment(ROOM_ID, EQUIPMENT_LIST)
room_equipment(0, [projector]).
room_equipment(1, [smart_board]).
room_equipment(1, [handicapped_access]).

% course_capacity(COURSE_ID, CAPACITY)
course_capacity(0, 30).
course_capacity(1, 50).
course_capacity(2, 80).
course_capacity(3, 90).
course_capacity(4, 150).

% course_time(COURSE_ID, HOURS)
course_time(0, 1).
course_time(1, 2).
course_time(2, 2).
course_time(3, 1).
course_time(4, 1).

% course_instructor(COURSE_ID, INSTRUCTOR)
course_instructor(0, 0).
course_instructor(1, 0).
course_instructor(2, 4).
course_instructor(3, 1).
course_instructor(4, 2).

% course_equipments(COURSE_ID, EQUIPMENT_LIST)
course_equipments(0, [handicapped_access]).
course_equipments(1, [projector, handicapped_access]).
course_equipments(2, [smart_board, handicapped_access]).
course_equipments(3, []).
course_equipments(4, []).

% course_schedule(ROOM_ID, START_HOUR, COURSE_ID)
course_schedule(0, 16, 0).
course_schedule(0, 17, 0).
course_schedule(0, 12, 1).
course_schedule(0, 12, 4).
course_schedule(0, 14, 2).
course_schedule(1, 12, 3).
course_schedule(1, 12, 2).
course_schedule(1, 11, 4).

% course_students(COURSE_ID, STUDENT_ID_LIST)
course_students(0, [0, 1, 2]).
course_students(1, [1, 2]).
course_students(2, [0, 2]).
course_students(3, [0, 1]).
course_students(4, [2]).

% instructor(INSTRUCTOR_ID, NAME)
instructor(0, ahmet).
instructor(1, kemal).
instructor(2, ali).
instructor(3, mehmet).
instructor(4, mahmut).

% student(STUDENT_ID, NAME)
student(0, ayse).
student(1, fatma).
student(2, hayriye).

% course(COURSE_ID, NAME)
course(0, science).
course(1, history).
course(2, language).
course(3, tennis).
course(4, basketball).

% handicapped_students(STUDENT_ID_LIST)
handicapped_students([0]).

% part 2
schedule(istanbul, rize, 4).
schedule(istanbul, ankara, 1).
schedule(istanbul, izmir, 2).
schedule(izmir, ankara, 6).
schedule(izmir, antalya, 2).
schedule(rize, ankara, 5).
schedule(ankara, van, 4).
schedule(ankara, diyarbakir, 8).
schedule(van, gaziantep, 3).
schedule(antalya, diyarbakir, 4).
schedule(antalya, erzincan, 3).
schedule(canakkale, erzincan, 6).

schedule(sakarya, kocaeli, 2).
schedule(kocaeli, istanbul, 1).

% rules

% part 1
instructor_courses(ID, COURSES) :-
    instructor(ID, NAME),
    instructor_courses(instructor(ID, NAME), COURSES).

instructor_courses(instructor(ID, NAME), COURSES) :-
    instructor_courses(instructor(ID, NAME), COURSES, []).

instructor_courses(instructor(ID, NAME), [], _) :-
    instructor(ID, NAME).

instructor_courses(instructor(ID, NAME), [course(C,N)|T], V) :-
    course_instructor(C, ID),
    course(C, N),
    instructor(ID, NAME),
    \+ member(C, V),
    instructor_courses(instructor(ID, NAME), T, [C|V]).


student_attends(ID, COURSES) :-
    student(ID, NAME),
    student_attends(student(ID, NAME), COURSES).

student_attends(student(ID, NAME), COURSES) :-
    student_attends(student(ID, NAME), COURSES, []).

student_attends(student(ID, NAME), [], _) :-
    student(ID, NAME).

student_attends(student(ID, NAME), [course(C,N)|T], V) :-
    student(ID, NAME),
    course_students(C, STUDENTS),
    course(C, N),
    \+ member(C, V),
    member(ID, STUDENTS),
    student_attends(student(ID, NAME), T, [C|V]).


student_handicapped(ID) :-
    student(ID, NAME),
    student_handicapped(student(ID, NAME)).

student_handicapped(student(ID, NAME)) :-
    student(ID, NAME),
    handicapped_students(STUDENTS),
    member(ID, STUDENTS).


course_schedule_conflicts([course_schedule(R,H,NAME1),course_schedule(R,H,NAME2)]) :-
    course_schedule(R, H, C1),
    course_schedule(R, H, C2),
    course(C1, NAME1),
    course(C2, NAME2),
    C1 \= C2.

course_schedule_conflicts([course_schedule(R,H,NAME1),course_schedule(R,H,NAME2),course_schedule(R,H,NAME3)]) :-
    course_schedule(R, H, C1),
    course_schedule(R, H, C2),
    course_schedule(R, H, C3),
    course(C1, NAME1),
    course(C2, NAME2),
    course(C3, NAME3),
    C1 \= C2,
    C2 \= C3,
    C1 \= C3.

course_schedule_conflicts(S) :-
    course_schedule_conflicts(S, []).

course_schedule_conflicts([course_schedule(R,H,NAME1),course_schedule(R,H,NAME2)|T], V) :-
    course_schedule(R, H, C1),
    course_schedule(R, H, C2),
    C1 \= C2,
    \+ member(course_schedule(R,H,C1), V),
    \+ member(course_schedule(R,H,C2), V),
    course(C1, NAME1),
    course(C2, NAME2),
    course_schedule_conflicts(T, [course_schedule(R,H,C1),course_schedule(R,H,C2)|V]).


can_enroll(ID, course(C, N)) :-
    student(ID, NAME),
    can_enroll(student(ID, NAME), course(C, N)).

can_enroll(student(ID, NAME), course(C, N)) :-
    student(ID, NAME),
    course(C, N),
    course_students(C, IDS),
    \+ member(ID, IDS),
    length(IDS, L),
    course_capacity(C, CC),
    L < CC,
    \+ student_handicapped(ID).

can_enroll(student(ID, NAME), course(C, N)) :-
    student(ID, NAME),
    course(C, N),
    course_students(C, IDS),
    \+ member(ID, IDS),
    length(IDS, L),
    course_capacity(C, CC),
    L < CC,
    student_handicapped(ID),
    course_equipments(C, EQS),
    member(handicapped_access, EQS).


room_available_hours(ID, HOURS) :-
    room_available_hours(room(ID), HOURS).

room_available_hours(room(ID), HOURS) :-
    room_available_hours(room(ID), HOURS, 8).

room_available_hours(room(_), [], 18).

room_available_hours(room(ID), HOURS, T) :-
    T < 18,
    course_schedule(ID, T, _),
    TI is T + 1,
    room_available_hours(room(ID), HOURS, TI).

room_available_hours(room(ID), [T|HOURS], T) :-
    T < 18,
    \+ course_schedule(ID, T, _),
    TI is T + 1,
    room_available_hours(room(ID), HOURS, TI).



% assignable_room_courses(ID, COURSES) :-
%     assignable_room_courses(room(ID), COURSES).

% assignable_room_courses(room(ID), COURSES) :-
%     assignable_room_courses(room(ID), COURSES, []).

% assignable_room_courses(room(ID), [course(C,N)|T], V) :-



% part 2
bischedule(X, Y, C) :-
    schedule(X, Y, C).

bischedule(X, Y, C) :-
    schedule(Y, X, C).

connection(X, Y, C) :-
    connection(X, Y, [X], C).

connection(X, Y, _, C) :-
    bischedule(X, Y, C).

connection(X, Y, V, C) :-
    bischedule(X, Z, C1),
    \+ member(Z, V),
    connection(Z, Y, [Z|V], C2),
    X \= Y,
    C is C1 + C2.
