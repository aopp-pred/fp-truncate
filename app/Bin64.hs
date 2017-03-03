--
-- Copyright 2017 Andrew Dawson
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
import Truncate              (makeBin64)
import Truncate.Main         (btMain)
import Truncate.Main.Readers (binary)

description :: String
description = "Truncate 64-bit floating-point numbers represented in binary"

main :: IO ()
main = btMain binary makeBin64 description
