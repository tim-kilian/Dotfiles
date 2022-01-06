#!/bin/env bash

(killall plank || echo 0) && plank &
