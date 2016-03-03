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

public class TexScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.TEX, new Line(Language.TEX, BLANK), "\n");
        assertLine(Language.TEX, new Line(Language.TEX, BLANK), "     \n");
        assertLine(Language.TEX, new Line(Language.TEX, BLANK), "\t\n");
        assertLine(Language.TEX, new Line(Language.TEX, CODE), "$$-b \\pm \\sqrt{b^2 - 4ac} \\over 2a$$\n");
        assertLine(Language.TEX, new Line(Language.TEX, COMMENT), "% Line comment\n");
        assertLine(Language.TEX, new Line(Language.TEX, COMMENT), "%\n");
        assertLine(Language.TEX, new Line(Language.TEX, CODE), "$$-b \\pm \\sqrt{b^2 - 4ac} \\over 2a$$\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.TEX, new Line(Language.TEX, BLANK), "     ");
        assertLine(Language.TEX, new Line(Language.TEX, BLANK), "\t");
        assertLine(Language.TEX, new Line(Language.TEX, CODE), "$$-b \\pm \\sqrt{b^2 - 4ac} \\over 2a$$");
        assertLine(Language.TEX, new Line(Language.TEX, COMMENT), "% Line comment");
        assertLine(Language.TEX, new Line(Language.TEX, COMMENT), "%");
        assertLine(Language.TEX, new Line(Language.TEX, CODE), "$$-b \\pm \\sqrt{b^2 - 4ac} \\over 2a$$");
    }

    @Test
    public void sampleTest() {
        String code = "\\documentclass[12pt]{article}\n"
                + "\\usepackage{amsmath}\n"
                + "\\title{\\LaTeX}\n"
                + "\\date{}\n"
                + "\\begin{document}\n"
                + "		\\maketitle\n"
                + "		``\\LaTeX{} is a document preparation system for the \\TeX{}\n"
                + "		typesetting program.''\n"
                + "		\n"
                + "		% This is a comment; it will not be shown in the final output.\n"
                + "		% The following shows a little of the typesetting power of LaTeX:\n"
                + "		\\begin{align}\n"
                + "			E &= mc^2                              \\\\ \n"
                + "			m &= \\frac{m_0}{\\sqrt{1-\\frac{v^2}{c^2}}}\n"
                + "		\\end{align}\n"
                + "\\end{document}\n";

        Line[] expected = {
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, BLANK),
                new Line(Language.TEX, COMMENT),
                new Line(Language.TEX, COMMENT),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
        };
        assertLines(Language.TEX, expected, code);
    }

}
