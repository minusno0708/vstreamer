'use client'

import { VideoPlayer } from "@/app/_components/Video";
import styles from "@/app/page.module.css";
import { useParams } from 'next/navigation'

export default function Home() {
  const { id } = useParams<{ id: string }>();

  return (
    <div className={styles.page}>
      <main className={styles.main}>
        <h1>Videos</h1>
        <VideoPlayer id={id} />
        <a href="/">Home</a>
      </main>
    </div>
  );
}
