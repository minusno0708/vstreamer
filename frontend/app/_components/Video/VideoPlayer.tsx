'use client'

import { useEffect, useRef } from 'react'

type VideoPlayerProps = {
    id: string;
};

export default function VideoPlayer(video: VideoPlayerProps) {
    const src = `http://localhost:8080/contents/videos/${video.id}/manifest.mpd`;

    const videoRef = useRef<HTMLVideoElement | null>(null)

    useEffect(() => {
        let player: any

        const initDash = async () => {
            console.log('Initializing DASH player for:', src)

            const dashjs = await import('dashjs')
            player = dashjs.MediaPlayer().create()
            player.initialize(videoRef.current, src, true)
        }

        if (videoRef.current) {
            initDash()
        }
        
        return () => {
            player?.reset()
        }
    }, [src])

    return (
        <video
            ref={videoRef}
            controls
            style={{ width: '100%', maxWidth: '800px' }}
        />
    );
}