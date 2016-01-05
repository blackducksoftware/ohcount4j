/**
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
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class CStyleScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.C, new Line(Language.C, BLANK), "\n");
        assertLine(Language.C, new Line(Language.C, BLANK), "     \n");
        assertLine(Language.C, new Line(Language.C, BLANK), "\t\n");
        assertLine(Language.C, new Line(Language.C, CODE), "#include <stdio.h>\n");
        assertLine(Language.C, new Line(Language.C, COMMENT), "/* Block Comment */\n");
        assertLine(Language.C, new Line(Language.C, COMMENT), "// Line comment\n");
        assertLine(Language.C, new Line(Language.C, COMMENT), "//\n");
        assertLine(Language.C, new Line(Language.C, CODE), "#include <stdio.h> // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.C, new Line(Language.C, BLANK), "     ");
        assertLine(Language.C, new Line(Language.C, BLANK), "\t");
        assertLine(Language.C, new Line(Language.C, CODE), "#include <stdio.h>");
        assertLine(Language.C, new Line(Language.C, COMMENT), "/* Block Comment */");
        assertLine(Language.C, new Line(Language.C, COMMENT), "// Line comment");
        assertLine(Language.C, new Line(Language.C, COMMENT), "//");
        assertLine(Language.C, new Line(Language.C, CODE), "#include <stdio.h> // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "#include <stdio.h>\n"
                + "\n"
                + "main() {\n"
                + "  printf(\"Hello world!\");\n"
                + "}";

        Line[] expected = {
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, CODE),
                new Line(Language.C, BLANK),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE)
        };
        assertLines(Language.C, expected, code);
    }

    @Test
    public void helloWorldWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("cstyle-1.c")));
        Line[] expected = {
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, COMMENT),
                new Line(Language.C, CODE),
                new Line(Language.C, BLANK),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE)
        };
        assertLines(Language.C, expected, sourceFile);
    }

    @Test
    public void helloWorldWithSourceFileMoreThenWith5KB() throws Exception {
        String c10Bytes = "// c \n"
                + "\n"
                + "#include<stdio.h>\n"
                + "main() {\n"
                + "  printf(\"1\");\n"
                + "}\n";
        byte[] byteArray = c10Bytes.getBytes();
        // will create 3.5MB file
        File tempFile = createTempFile();
        try (OutputStream outputStream = new FileOutputStream(tempFile)) {
            for (int i = 0; i < 1024 * 75; i++) {
                outputStream.write(byteArray);
            }
        } // 6 lines
        SourceFile sourceFile = new SourceFile(new File(tempFile.getAbsolutePath()));
        Line[] expected = {
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE),
                new Line(Language.C, CODE)
        };
        List<Line> expectedLines = new ArrayList<Line>();
        for (int i = 0; i < 1024 * 75 * expected.length; i++) {
            expectedLines.add(expected[i % expected.length]);
        }
        assertLines(Language.C, expectedLines.toArray(new Line[expectedLines.size()]), sourceFile);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.C, CODE),
                new Line(Language.C, CODE),
                new Line(Language.C, BLANK)
        };
        assertLines(Language.C, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.C, COMMENT),
                new Line(Language.C, BLANK),
                new Line(Language.C, BLANK)
        };
        assertLines(Language.C, expected, code);
    }
}
