import Link from 'next/link'
import { VideoProps } from '.';

export default function VideoCard(video: VideoProps) {
    return (
        <div>
            <Link href={`/videos/${video.id}`}>{video.name}</Link>
        </div>
    )
}