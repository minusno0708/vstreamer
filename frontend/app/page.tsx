import Image from "next/image";
import styles from "./page.module.css";
import VideoList from "./_components/Video/VideoList";

export default function Home() {
  return (
    <div className={styles.page}>
      <main className={styles.main}>
        <h1>Vstreamer</h1>
        <a href="/upload">Upload</a>

        < VideoList />
      </main>
    </div>
  );
}
