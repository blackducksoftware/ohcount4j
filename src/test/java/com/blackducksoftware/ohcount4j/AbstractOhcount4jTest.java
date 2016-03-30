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
package com.blackducksoftware.ohcount4j;

import static java.io.File.separator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.AfterMethod;

public abstract class AbstractOhcount4jTest {

    private static final String TEMP_SUFFIX = ".srcfile";

    private static final String TEMP_PREFIX = "____";

    private List<File> tempFiles = new ArrayList<File>();

    @AfterMethod
    public void afterTest() {
        for (File tf : tempFiles) {
            if (tf.exists() && !tf.delete()) {
                System.err.println("Could not delete temp file " + tf);
            }
        }
    }

    protected String getSourceCodePath(String fileName) {
        StringBuilder srcPath = new StringBuilder(System.getProperty("user.dir"));

        srcPath.append(separator).append("src").append(separator).append("test").
                append(separator).append("src-code").append(separator).append(fileName);

        return srcPath.toString();
    }

    protected File createTempFile() {
        try {
            File tempFile = File.createTempFile(TEMP_PREFIX, TEMP_SUFFIX);
            tempFiles.add(tempFile);
            return tempFile;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
