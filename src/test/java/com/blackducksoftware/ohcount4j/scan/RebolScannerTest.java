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

public class RebolScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.REBOL, new Line(Language.REBOL, BLANK), "\n");
        assertLine(Language.REBOL, new Line(Language.REBOL, BLANK), "     \n");
        assertLine(Language.REBOL, new Line(Language.REBOL, BLANK), "\t\n");
        assertLine(Language.REBOL, new Line(Language.REBOL, CODE), "while [not tail? mail] [\n");
        assertLine(Language.REBOL, new Line(Language.REBOL, COMMENT), "; Line comment\n");
        assertLine(Language.REBOL, new Line(Language.REBOL, COMMENT), ";\n");
        assertLine(Language.REBOL, new Line(Language.REBOL, CODE), "foreach page pages [send boss@hans.dom read page] // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.REBOL, new Line(Language.REBOL, BLANK), "     ");
        assertLine(Language.REBOL, new Line(Language.REBOL, BLANK), "\t");
        assertLine(Language.REBOL, new Line(Language.REBOL, CODE), "while [not tail? mail] [");
        assertLine(Language.REBOL, new Line(Language.REBOL, COMMENT), "; Line comment");
        assertLine(Language.REBOL, new Line(Language.REBOL, COMMENT), ";");
        assertLine(Language.REBOL, new Line(Language.REBOL, CODE), "foreach page pages [send boss@hans.dom read page] // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "; Simple Rebol Code\n"
                + "view layout [\n"
                + "		a: area\n"
                + "		btn \"Save\" [\n"
                + "			write %reboltut.txt a/text\n"
                + "			alert \"Saved\"\n"
                + "			alert {Random String using\n"
                + "					curley braces {\n"
                + "\t\t\n"
                + "					; comment inside string is string\n"
                + "					can be nested {\n"
                + "						many times }}\n"
                + "				}\n"
                + "		; comment after string is comment\n"
                + "		]\n"
                + "]\n";

        Line[] expected = {
                new Line(Language.REBOL, COMMENT),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, BLANK),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, COMMENT),
                new Line(Language.REBOL, CODE),
                new Line(Language.REBOL, CODE)
        };
        assertLines(Language.REBOL, expected, code);
    }
}
