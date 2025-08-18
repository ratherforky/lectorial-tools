#!/usr/bin/env run-script
module Application.Script.ClearDB where

import Application.Script.Prelude

run :: Script
run = do
  deleteAll @RoomsStudentsSelected
  deleteAll @RoomsStudent
  deleteAll @Student
  deleteAll @Room

