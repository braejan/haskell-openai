-- haskell-openai
-- Copyright (c) 2024 https://github.com/braejan
--
-- This Source Code Form is subject to the terms of the MIT License.
-- If a copy of the MIT was not distributed with this file, You can
-- obtain one at http://opensource.org/licenses/MIT.
--
-- The Haskell source code contained in this file is provided "as is",
-- without any express or implied warranty. Any use, reproduction or
-- modification of the source code constitutes the sole responsibility
-- of the person performing such action.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
-- ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
-- CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Configuration.Configuration where
import Data.Text ( Text )

data Configuration = Configuration 
  { apiKey :: Text
   ,organization :: Text
  }


createEmptyConfiguration :: Configuration
createEmptyConfiguration = Configuration {
  apiKey = "",
  organization = ""
}