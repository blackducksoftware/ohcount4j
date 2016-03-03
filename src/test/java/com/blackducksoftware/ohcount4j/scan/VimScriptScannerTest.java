/*
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class VimScriptScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK), "\n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK), "     \n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK), "\t\n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE), "if exists(\"g:syntax_on\")\n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\" Line comment\n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\"\n");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE), "if exists(\"g:syntax_on\") \" with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK), "     ");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, BLANK), "\t");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE), "if exists(\"g:syntax_on\")");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\" Line comment");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, COMMENT), "\"");
        assertLine(Language.VIMSCRIPT, new Line(Language.VIMSCRIPT, CODE), "if exists(\"g:syntax_on\") \" with comment");
    }

    @Test
    public void sampleTest() {
        String code = "\" Sample Script Written in Vimscript\n"
                + "let i = 1\n"
                + "while i < 5\n"
                + "  echo \"count is\" i\n"
                + "  let i += 1\n"
                + "endwhile\n";

        Line[] expected = {
                new Line(Language.VIMSCRIPT, COMMENT),
                new Line(Language.VIMSCRIPT, CODE),
                new Line(Language.VIMSCRIPT, CODE),
                new Line(Language.VIMSCRIPT, CODE),
                new Line(Language.VIMSCRIPT, CODE),
                new Line(Language.VIMSCRIPT, CODE)
        };
        assertLines(Language.VIMSCRIPT, expected, code);
    }

}
