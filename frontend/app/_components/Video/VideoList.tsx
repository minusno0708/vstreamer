"use client";

import { useState, useEffect } from 'react';
import { VideoProps, VideoCard } from '.';

export default function VideoList() {
    const [videos, setVideos] = useState<VideoProps[]>([]);
    const [loading, setLoading] = useState(false);

    const fetchVideos = async () => {
        setLoading(true);
        try {
            const response = await fetch('http://localhost:8080/api/videos');
            if (!response.ok) {
                throw new Error('Failed to fetch videos');
            }
            const data = await response.json();
            setVideos(data);
        } catch (error) { 
            console.log('Error fetching videos:', error);
        }
        setLoading(false);
    }

    useEffect(() => {
        fetchVideos();
    }, []);

    return (
        <div>
            {loading ? (
                <p>Loading...</p>
            ) : (
                <ul>
                    {videos.map((video) => (
                        VideoCard(video)
                    ))}
                </ul>
            )}
        </div>
    )
}