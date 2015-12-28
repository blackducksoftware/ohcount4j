/*
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package com.blackducksoftware.ohcount4j;

import java.io.IOException;

/**
 * @author mpujari
 *
 */
public final class SourceFileUtils {

    public static final int HEAD_SIZE = 100;

    private SourceFileUtils() {
    }

    public static String head(SourceFile sourceFile) throws IOException {
        if (sourceFile.isContentsFromFile()) {
            /*
             * we have a reader initialized, we should not use it as it will
             * forward the reader and we don't want that
             */
            try (SourceFile srcFile = new SourceFile(sourceFile.getPath())) {
                return srcFile.head(HEAD_SIZE);
            }
        } else {
            return new String(sourceFile.getContents());
        }
    }

    public static CharSequence getCharSequence(SourceFile sourceFile) throws IOException {
        return sourceFile.getCharSequence();
    }

    public static char[] getContents(SourceFile sourceFile) throws IOException {
        return sourceFile.getContents();
    }

}
