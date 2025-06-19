'use client'

import { useState, useEffect } from 'react';
import { VideoProps, VideoPlayer } from "@/app/_components/Video";
import styles from "@/app/page.module.css";
import { useParams } from 'next/navigation'

export default function Home() {
	const { id } = useParams<{ id: string }>();

	const [video, setVideo] = useState<VideoProps | null>(null);

	const fetchVideo = async () => {
		try {
			const res = await fetch(`http://localhost:8080/api/videos/${id}`);
			if (!res.ok) {
				throw new Error('Failed to fetch videos');
			}
			const data = await res.json();
			setVideo(data);
		} catch (error) {
			console.log('Error fetching videos:', error);
		}
	}

	useEffect(() => {
		fetchVideo();
	}, [id]);

	return (
		<div className={styles.page}>
			<main className={styles.main}>
				{!video ? (
					<h1>Loading...</h1>
				) : (
					<h1>{video.name}</h1>
				)}
				<VideoPlayer src={`http://localhost:8080/contents/videos/${id}/manifest.mpd`} />
				<a href="/">Home</a>
			</main>
		</div>
	);
}
