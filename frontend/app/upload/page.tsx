"use client";

import styles from "@/app/page.module.css";
import { redirect } from 'next/navigation'
import Form from 'next/form'

export default function Home() {
  return (
    <div className={styles.page}>
        <main className={styles.main}>
            <h1>Upload Videos</h1>
            <Form action={ handleUpload }>
                <input type="file" name="video" accept="video/*" />
                <button type="submit">Upload</button>
            </Form>
            <a href="/">Home</a>
        </main>
    </div>
  );
}

async function handleUpload(formData: FormData) {
    if (!formData.has("video")) {
        throw new Error("No video file provided");
    }
    const res = await fetch("http://localhost:8080/api/videos", {
        method: "POST",
        body: formData,
    });

    if (!res.ok) {
        throw new Error("Failed to upload video");
    }

    redirect("/")
}