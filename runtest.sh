#!/bin/bash

erl -make && erl -pa ebin -s runtest run -s init stop
