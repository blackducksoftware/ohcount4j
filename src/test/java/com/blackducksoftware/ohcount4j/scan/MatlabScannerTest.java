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

public class MatlabScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.MATLAB, new Line(Language.MATLAB, BLANK), "\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, BLANK), "     \n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, BLANK), "\t\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, CODE), "Horiz = [1,2,3];\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "%{ Block comment }%\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "% Line comment\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "...Line comment\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "%\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "...\n");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, CODE), "f = inline('2*x*y', 'x', 'y'); % with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.MATLAB, new Line(Language.MATLAB, BLANK), "     ");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, BLANK), "\t");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, CODE), "Horiz = [1,2,3];");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "%{ Block comment }%");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "% Line comment");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "...Line comment");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "%");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, COMMENT), "...");
        assertLine(Language.MATLAB, new Line(Language.MATLAB, CODE), "f = inline('2*x*y', 'x', 'y'); % with comment");
    }

    @Test
    public void sampleTest() {
        String code = "%{ Multi\n"
                + "\t\n"
                + "line comment %}\n"
                + "for n= 3:length(Xw),\n"
                + "	    y(n)=sum(Xw(n-2:n))/3;       %y[n] is the filtered signal\n"
                + "end\n"
                + "\n"
                + "% Single Line Comment\n"
                + "			...continued on next line with line continue function\n"
                + "plot(y);\n"
                + "hold;\n"
                + "%";

        Line[] expected = {
                new Line(Language.MATLAB, COMMENT),
                new Line(Language.MATLAB, BLANK),
                new Line(Language.MATLAB, COMMENT),
                new Line(Language.MATLAB, CODE),
                new Line(Language.MATLAB, CODE),
                new Line(Language.MATLAB, CODE),
                new Line(Language.MATLAB, BLANK),
                new Line(Language.MATLAB, COMMENT),
                new Line(Language.MATLAB, COMMENT),
                new Line(Language.MATLAB, CODE),
                new Line(Language.MATLAB, CODE),
                new Line(Language.MATLAB, COMMENT)
        };
        assertLines(Language.MATLAB, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "%{\n\n\n";

        Line[] expected = {
                new Line(Language.MATLAB, COMMENT),
                new Line(Language.MATLAB, BLANK),
                new Line(Language.MATLAB, BLANK)
        };
        assertLines(Language.MATLAB, expected, code);
    }
}
