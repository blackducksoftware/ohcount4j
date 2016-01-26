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

public class AugeasScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "     \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\t\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "module Modules =\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "autoload xfm\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, COMMENT), "(* Block Comment *)\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let word = /[^#, \\n\\t\\/]+/ \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let record = [ key word . (Util.del_ws_tab . sto_line)? . Util.eol ] \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let filter = incl \"/etc/modules\"\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "test lns get \"key = value\" = ?\n");
    }

    @Test
    public void helloWorld() {
        String code = "(* Hello World\n" // Multi-line comment
                + " * with multi-line comment *)\n"
                + "\n"
                + "(* Hello \n" // Nested Comments
                + "(* World \n"
                + " * with *)\n"
                + "nested comment *)\n"
                + "\n"
                + "package main\n"
                + "\n"
                + "import \"fmt\"\n"
                + "\n"
                + "\tfunc main() { \n"
                + "(* single line comment *)\n" // single line comment
                + "\t\tvar x float64 = 20.0\n"
                + "\t\tfmt.Println(\"Hello, World!\")\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE)
        };
        assertLines(Language.AUGEAS, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "     ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\t");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "module Modules =");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "autoload xfm");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, COMMENT), "(* Block Comment *)");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let word = /[^#, \\n\\t\\/]+/ ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let record = [ key word . (Util.del_ws_tab . sto_line)? . Util.eol ] ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let filter = incl \"/etc/modules\"");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "test lns get \"key = value\" = ?");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        String code = "\"\nA\n\n";

        Line[] expected = {
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK)
        };
        assertLines(Language.AUGEAS, expected, code);
    }

}
