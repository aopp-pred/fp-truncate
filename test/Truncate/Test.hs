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
module Truncate.Test (suites) where


------------------------------------------------------------------------
import Test.Framework (Test)


------------------------------------------------------------------------
import qualified Truncate.Test.Truncatable.FromBits
import qualified Truncate.Test.Truncatable.MakeBits
import qualified Truncate.Test.Truncatable.Convert
import qualified Truncate.Test.Truncatable.Operator
import qualified Truncate.Internal.Test.TruncateWord


------------------------------------------------------------------------
-- Test suites for the 'Truncate' module
suites :: [Test]
suites = [ Truncate.Test.Truncatable.FromBits.suite
         , Truncate.Test.Truncatable.MakeBits.suite
         , Truncate.Test.Truncatable.Convert.suite
         , Truncate.Test.Truncatable.Operator.suite
         , Truncate.Internal.Test.TruncateWord.suite
         ]
