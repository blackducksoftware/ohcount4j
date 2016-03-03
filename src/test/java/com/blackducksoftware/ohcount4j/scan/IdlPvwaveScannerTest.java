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

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class IdlPvwaveScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, BLANK), "\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, BLANK), "     \n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, BLANK), "\t\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "if not keyword_set(base) then base = 16\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "pro showfont, num, name, encapsulated=eps\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "plot,  [h_x, v_x], [h_y, v_y], $\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, COMMENT), "; Line comment\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "for c = beg, fin do $\n");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "end\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, BLANK), "     ");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, BLANK), "\t");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "if not keyword_set(base) then base = 16");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "pro showfont, num, name, encapsulated=eps");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "plot,  [h_x, v_x], [h_y, v_y], $");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, COMMENT), "; Line comment");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "for c = beg, fin do $");
        assertLine(Language.IDL_PVWAVE, new Line(Language.IDL_PVWAVE, CODE), "end");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("idl_pvwave.pro")));
        Line[] expected = {
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, COMMENT),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, CODE),
                new Line(Language.IDL_PVWAVE, BLANK),
                new Line(Language.IDL_PVWAVE, CODE),
        };
        assertLines(Language.IDL_PVWAVE, expected, sourceFile);
    }

}
