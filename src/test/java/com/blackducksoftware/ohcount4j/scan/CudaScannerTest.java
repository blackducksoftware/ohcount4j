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
import static com.blackducksoftware.ohcount4j.Language.CUDA;

import org.testng.annotations.Test;

public class CudaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(CUDA, new Line(CUDA, BLANK), "\n");
        assertLine(CUDA, new Line(CUDA, BLANK), "     \n");
        assertLine(CUDA, new Line(CUDA, BLANK), "\t\n");
        assertLine(CUDA, new Line(CUDA, CODE), "public:\n");
        assertLine(CUDA, new Line(CUDA, COMMENT), "/* Block Comment */\n");
        assertLine(CUDA, new Line(CUDA, COMMENT), "// Line comment\n");
        assertLine(CUDA, new Line(CUDA, COMMENT), "//\n");
        assertLine(CUDA, new Line(CUDA, CODE), "#include <stdio.h> // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(CUDA, new Line(CUDA, BLANK), "     ");
        assertLine(CUDA, new Line(CUDA, BLANK), "\t");
        assertLine(CUDA, new Line(CUDA, CODE), "#include <iostream>");
        assertLine(CUDA, new Line(CUDA, COMMENT), "/* Block Comment */");
        assertLine(CUDA, new Line(CUDA, COMMENT), "// Vector(const Vector &v)");
        assertLine(CUDA, new Line(CUDA, COMMENT), "//");
        assertLine(CUDA, new Line(CUDA, CODE), "#include <stdio.h> // with comment");
        assertLine(CUDA, new Line(CUDA, CODE), "private: // with comment");
        assertLine(CUDA, new Line(CUDA, CODE), "class Vector { // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "#ifndef __VECTOR_CUH__\n"
                + "#define __VECTOR_CUH__\n"
                + "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "#include <stdio.h>\n"
                + "\n"
                + " __device__ __host__ unsigned int getSize() const { return size_; }\n";

        Line[] expected = {
                new Line(CUDA, CODE),
                new Line(CUDA, CODE),
                new Line(CUDA, COMMENT),
                new Line(CUDA, COMMENT),
                new Line(CUDA, BLANK),
                new Line(CUDA, CODE),
                new Line(CUDA, BLANK),
                new Line(CUDA, CODE)
        };
        assertLines(CUDA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(CUDA, CODE),
                new Line(CUDA, CODE),
                new Line(CUDA, BLANK)
        };
        assertLines(CUDA, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(CUDA, COMMENT),
                new Line(CUDA, BLANK),
                new Line(CUDA, BLANK)
        };
        assertLines(CUDA, expected, code);
    }
}
